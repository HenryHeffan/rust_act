use super::basic::{ast::*, *};
use crate::parser::utils::{MyParserExt, ParserExt2, EE};
use nom::{
    branch::alt,
    combinator::{map, not, opt, peek},
    sequence::{pair, preceded, terminated, tuple},
    IResult, Parser,
};
use nom_supreme::parser_ext::ParserExt;

mod lang_chp_hse {
    use super::*;
    use crate::parser::utils::Unterm;

    pub mod ast {
        use super::*;

        pub type LangChp<'a> = (Option<SupplySpec<'a>>, Option<ChpItemList<'a>>);
        pub type ChpItemList<'a> = Vec<Vec<ChpItem<'a>>>;

        #[derive(Debug, Copy, Clone)]
        pub enum SemiOrComma {
            Semi,
            Comma,
        }

        #[derive(Debug)]
        pub enum ChpItem<'a> {
            Stmt(Option<Ident<'a>>, ChpStmt<'a>),
            MacroLoop(MacroLoop<'a, SemiOrComma, (), ChpItemList<'a>>),
        }

        #[derive(Debug)]
        pub enum AssignStmt<'a> {
            Assign(ExprId<'a>, Expr<'a>),
            Bool(ExprId<'a>, Dir),
        }

        #[derive(Debug)]
        pub enum ChpStmt<'a> {
            Assign(AssignStmt<'a>),
            SendStmt(SendStmt<'a>),
            RecvStmt(RecvStmt<'a>),
            Skip,
            ParenedBody(ChpItemList<'a>),
            FuncCall(Ident<'a>, Vec<ExprOrStr<'a>>),
            DottedCall(BaseId<'a>, Ident<'a>, Vec<Expr<'a>>),
            Select(ChpSelectStmt<'a>),
            DoLoop(ChpDoLoop<'a>),
            WhileLoop(ChpWhileLoop<'a>),
        }

        #[derive(Debug, Copy, Clone)]
        pub enum SendType {
            Normal,
            Plus,
            Minus,
        }

        #[derive(Debug, Copy, Clone)]
        pub enum RecvType {
            Normal,
            Plus,
            Minus,
        }

        #[derive(Debug)]
        pub enum ChpRecvTypeCast<'a> {
            Id(ExprId<'a>),
            AsBool(ExprId<'a>),
            AsInt(ExprId<'a>),
        }

        pub type SendStmt<'a> = (
            ExprId<'a>,
            SendType,
            Option<Expr<'a>>,
            Option<(RecvType, ChpRecvTypeCast<'a>)>,
        );
        pub type RecvStmt<'a> = (
            ExprId<'a>,
            RecvType,
            Option<ChpRecvTypeCast<'a>>,
            Option<(SendType, Expr<'a>)>,
        );

        #[derive(Debug)]
        pub enum GuardedCmd<'a> {
            Expr(Expr<'a>, ChpItemList<'a>),
            Else(ChpItemList<'a>),
            Macro(MacroLoop<'a, Ctrl<'a>, Expr<'a>, ChpItemList<'a>>),
        }

        #[derive(Debug)]
        pub enum ChpSelectStmt<'a> {
            Determ(Vec<GuardedCmd<'a>>),
            NonDeterm(Vec<GuardedCmd<'a>>),
            Expr(Expr<'a>),
        }

        pub type ChpDoLoop<'a> = (ChpItemList<'a>, Option<Expr<'a>>);
        pub type ChpWhileLoop<'a> = Vec<GuardedCmd<'a>>;

        // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
        #[derive(Debug)]
        pub enum HseBodies<'a> {
            Body(HseItemList<'a>),
            Labeled(Vec<(Ident<'a>, HseItemList<'a>, Ident<'a>)>),
        }

        pub type HseItemList<'a> = ChpItemList<'a>;
        pub type LangHse<'a> = (Option<SupplySpec<'a>>, Option<HseBodies<'a>>);
    }

    use ast::*;

    // chp_body: { chp_comma_list ";" }*
    // chp_comma_list: { chp_body_item "," }*
    // chp_body_item: [ ID ":" ] chp_basic_stmt
    //              | [ ID ":" ] chp_select_stmt
    //              | [ ID ":" ] chp_loop_stmt
    //              | "(" ";" ID ":" !noreal expr[ ".." expr ] ":" chp_body ")"
    //              | "(" "," ID ":" !noreal expr [ ".." expr ] ":" chp_body ")"
    // chp_basic_stmt: send_stmt
    //          | recv_stmt
    //          | assign_stmt
    //          | "skip"
    //          | "(" chp_body ")"
    //          | ID "(" { chp_log_item "," }* ")"
    //          | base_id "." ID "(" [ { expr "," }** ] ")"
    pub fn chp_item<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ChpItem<'a>, E> {
        let semi_or_comma = alt((
            ctrl(';').p().value(SemiOrComma::Semi),
            ctrl(',').p().value(SemiOrComma::Comma),
        ));
        let parened_body = chp_item_list1().parened();
        let func_call = ident.and(expr_or_str.list1_sep_by(ctrl(',')).parened());
        let dotted_call = base_id
            .then_ignore(ctrl('.'))
            .then(ident)
            .then(expr.list1_sep_by(ctrl(',')).parened());
        let chp_stmt = alt((
            send_stmt.map(ChpStmt::SendStmt),
            recv_stmt.map(ChpStmt::RecvStmt),
            assign_stmt.map(ChpStmt::Assign),
            kw("skip").map(|_| ChpStmt::Skip),
            parened_body.map(ChpStmt::ParenedBody),
            func_call.map(|(a, b)| ChpStmt::FuncCall(a, b)), // TODO make this a list0_sep_by
            dotted_call.map(|((a, b), c)| ChpStmt::DottedCall(a, b, c)),
            chp_select_stmt.map(|a| ChpStmt::Select(a)),
            chp_do_or_while_loop.map(|a| match a {
                ChpDoOrWhileLoop::Do(a) => ChpStmt::DoLoop(a),
                ChpDoOrWhileLoop::While(a) => ChpStmt::WhileLoop(a),
            }),
        ));
        let labeled_stmt = ident.then_ignore(ctrl(':')).opt().then(chp_stmt);
        alt((
            // special case to make sure it parses unlabeled assignments correctly
            assign_stmt.map(|stmt| ChpItem::Stmt(None, ChpStmt::Assign(stmt))),
            // then the general case
            labeled_stmt.map(|(label, stmt)| ChpItem::Stmt(label, stmt)),
            ctrl('(')
                .precedes(tuple((
                    semi_or_comma,
                    ident,
                    preceded(ctrl(':'), expr_range),
                    preceded(ctrl(':'), none),
                    chp_item_list1().terminated(ctrl(')')),
                )))
                .map(ChpItem::MacroLoop),
        ))
        .parse(i)
    }

    fn chp_item_list1<'a, E: EE<'a>>() -> Unterm<impl Parser<&'a [u8], Vec<ChpItem<'a>>, E>> {
        let chp_comma_list = chp_item.list1_sep_by(ctrl(',')).p();
        chp_comma_list.list1_sep_by(ctrl(';'))
    }

    // gen_assignable_id: {excl} expr_id
    //                  | "bool" "(" expr_id ")"
    //                  | "int" "(" expr_id ")"
    fn chp_recv_type_cast<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ChpRecvTypeCast<'a>, E> {
        alt((
            expr_id.map(ChpRecvTypeCast::Id),
            kw("bool").ignore_then(expr_id.parened()).map(ChpRecvTypeCast::AsBool),
            kw("int").ignore_then(expr_id.parened()).map(ChpRecvTypeCast::AsInt),
        ))
        .parse(i)
    }

    // snd_type: {excl}
    //          "!"
    //         | "!+"
    //         | "!-"
    // rcv_type: {excl}
    //          "?"
    //         | "?+"
    //         | "?-"
    // recv_stmt: expr_id rcv_type [ gen_assignable_id ] [ snd_type expr ]
    // send_stmt: expr_id snd_type [ expr ] [ rcv_type gen_assignable_id ]
    fn snd_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], SendType, E> {
        alt((
            ctrl2('!', '+').p().value(SendType::Plus),
            ctrl2('!', '-').p().value(SendType::Minus),
            ctrl('!').p().value(SendType::Normal),
        ))
        .parse(i)
    }

    fn rcv_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], RecvType, E> {
        alt((
            ctrl2('?', '+').p().value(RecvType::Plus),
            ctrl2('?', '-').p().value(RecvType::Minus),
            ctrl('?').p().value(RecvType::Normal),
        ))
        .parse(i)
    }

    fn send_stmt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], SendStmt<'a>, E> {
        expr_id
            .then(snd_type)
            .then(expr.opt())
            .then(rcv_type.then(chp_recv_type_cast).opt())
            .map(|(((a, b), c), d)| (a, b, c, d))
            .parse(i)
    }

    fn recv_stmt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], RecvStmt<'a>, E> {
        expr_id
            .then(rcv_type)
            .then(chp_recv_type_cast.opt())
            .then(snd_type.then(expr).opt())
            .map(|(((a, b), c), d)| (a, b, c, d))
            .parse(i)
    }

    // assign_stmt: expr_id ":=" expr
    //            | expr_id dir
    //            ;
    fn assign_stmt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], AssignStmt<'a>, E> {
        expr_id
            .then_ignore(ctrl2(':', '='))
            .then(expr)
            .map(|(a, b)| AssignStmt::Assign(a, b))
            .or(expr_id.then(dir).map(|(a, b)| AssignStmt::Bool(a, b)))
            .parse(i)
    }

    // guarded_cmd: {excl}
    //             expr "->" chp_body
    //            | "else" "->" chp_body
    //            | "(" "[]" ID ":" !noreal expr [ ".." expr ] ":" expr "->" chp_body ")"
    // chp_select_stmt:
    //              "[" { guarded_cmd "[]" }* "]"
    //            | "[|" { guarded_cmd "[]" }* "|]"
    //            | "[" expr "]"
    // chp_loop_stmt: "*[" chp_body [ "<-" expr ] "]"
    //          | "*[" { guarded_cmd "[]" }* "]"

    // TODO come up with a way to make this handle the specific expected terminators
    fn guarded_cmd<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], GuardedCmd<'a>, E> {
        alt((
            expr.then_ignore(ctrl2('-', '>'))
                .then(chp_item_list1().p())
                .map(|(a, b)| GuardedCmd::Expr(a, b)),
            kw("else")
                .ignore_then(ctrl2('-', '>'))
                .ignore_then(chp_item_list1().p())
                .map(GuardedCmd::Else),
            ctrl('(')
                .precedes(tuple((
                    ctrl2('[', ']'),
                    ident,
                    preceded(ctrl(':'), expr_range),
                    preceded(ctrl(':'), expr.then_ignore(ctrl2('-', '>'))),
                    chp_item_list1().terminated(ctrl(')')),
                )))
                .context("macro loop")
                .map(GuardedCmd::Macro),
        ))
        .parse(i)
    }

    fn chp_select_stmt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ChpSelectStmt<'a>, E> {
        let nondet_select_stmt = guarded_cmd
            .list1_sep_by(ctrl2('[', ']'))
            .delim_by(ctrl2('[', '|'), ctrl2('|', ']'));
        let det_select_stmt = guarded_cmd.list1_sep_by(ctrl2('[', ']')).bracketed();
        alt((
            nondet_select_stmt.map(ChpSelectStmt::NonDeterm),
            det_select_stmt.map(ChpSelectStmt::Determ),
            expr.bracketed().map(ChpSelectStmt::Expr),
        ))
        .parse(i)
    }

    enum ChpDoOrWhileLoop<'a> {
        Do(ChpDoLoop<'a>),
        While(ChpWhileLoop<'a>),
    }

    fn chp_do_or_while_loop<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ChpDoOrWhileLoop<'a>, E> {
        let do_loop_test = peek(chp_item.then(not(ctrl2('<', '-'))));
        let do_loop = do_loop_test
            .ignore_then_cut(
                chp_item_list1()
                    .term_by_recog_alt2(ctrl2('<', '-'), ctrl(']'))
                    .and(ctrl2('<', '-').precedes(expr).opt().terminated(ctrl(']'))),
            )
            .map(ChpDoOrWhileLoop::Do);
        let while_loop = guarded_cmd
            .list1_sep_by(ctrl2('[', ']'))
            .term_by(ctrl(']'))
            .map(ChpDoOrWhileLoop::While);
        ctrl2('*', '[').ignore_then_cut(do_loop.or(while_loop)).parse(i)
    }

    // hse_bodies: hse_body
    //           | labelled_hse_bodies
    // labelled_hse_bodies: { label_hse_fragment ";" }*
    // label_hse_fragment: ID ":" hse_body ":" ID
    // hse_body: { hse_comma_item ";" }*
    // hse_comma_item: { hse_body_item "," }*
    // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
    pub fn hse_body<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], HseItemList<'a>, E> {
        chp_item_list1().parse(i)
    }

    fn hse_bodies<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], HseBodies<'a>, E> {
        let labeled_body = ident
            .then_ignore(ctrl(':'))
            .then(hse_body)
            .then_ignore(ctrl(':'))
            .then(ident)
            .map(|((a, b), c)| (a, b, c));
        let labeled_bodies = labeled_body.list1_sep_by(ctrl(';')).p().map(HseBodies::Labeled);
        hse_body.map(HseBodies::Body).or(labeled_bodies).parse(i)
    }

    // lang_chp: "chp" [ supply_spec ] "{" [ chp_body ] "}"
    // lang_hse: "hse" [ supply_spec ] "{" [ hse_bodies ] "}"
    // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
    pub fn lang_hse<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangHse<'a>, E> {
        kw("hse")
            .ignore_then(supply_spec.opt())
            .then(hse_bodies.opt().braced())
            .context("hse block")
            .parse(i)
    }

    pub fn lang_chp<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangChp<'a>, E> {
        let block = supply_spec.opt().then(chp_item_list1().opt().braced());
        kw("chp").ignore_then_cut(block).context("chp block").parse(i)
    }
}

mod lang_prs {

    use super::*;

    pub mod ast {
        use super::*;

        #[derive(Debug, Clone, Copy)]
        pub enum ArrowKind {
            Minus,
            Equals,
            Hash,
        }

        pub type SizeSpec<'a> = (Expr<'a>, Option<Expr<'a>>, Option<Ident<'a>>, Option<Expr<'a>>);

        #[derive(Debug)]
        pub enum TreeSubcktSpec<'a> {
            Expr(Expr<'a>),
            Str(StrTok<'a>),
        }

        #[derive(Debug)]
        pub enum PrsItem<'a> {
            Rule(PrsExpr<'a>, ArrowKind, ExprId<'a>, Dir),
            AtRule(PrsExpr<'a>, ArrowKind, Ident<'a>, Dir),
            SubBlock(Ident<'a>, Option<TreeSubcktSpec<'a>>, PrsBody<'a>),
            MacroRule(MacroLoop<'a, (), (), PrsBody<'a>>),
            PassN(SizeSpec<'a>, (ExprId<'a>, ExprId<'a>, ExprId<'a>)),
            PassP(SizeSpec<'a>, (ExprId<'a>, ExprId<'a>, ExprId<'a>)),
            TransGate(SizeSpec<'a>, (ExprId<'a>, ExprId<'a>, ExprId<'a>, ExprId<'a>)),
        }

        pub type PrsBody<'a> = Vec<(Option<AttrList<'a>>, PrsItem<'a>)>;
        pub type LangPrs<'a> = (Option<SupplySpec<'a>>, bool, PrsBody<'a>);
    }

    use ast::*;

    // arrow: {excl}
    //       "->"
    //      | "=>"
    //      | "#>"
    fn arrow_kind<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ArrowKind, E> {
        alt((
            ctrl2('-', '>').p().value(ArrowKind::Minus),
            ctrl2('=', '>').p().value(ArrowKind::Equals),
            ctrl2('#', '>').p().value(ArrowKind::Hash),
        ))
        .parse(i)
    }

    // prs_body: {t-rec}
    //          [ attr_list ] prs_item prs_body
    //         | [ attr_list ] prs_item
    // prs_item:EXTERN[prs_expr] arrow expr_id dir
    //           |EXTERN[prs_expr] arrow "@" ID dir
    //           | ID [ tree_subckt_spec ] "{" prs_body "}"
    //           | "(" ID ":" !noreal expr [ ".." expr ] ":" prs_body ")"
    //           | "passn" size_spec "(" expr_id "," expr_id "," expr_id ")"
    //           | "passp" size_spec "(" expr_id "," expr_id "," expr_id ")"
    //           | "transgate" size_spec "(" expr_id "," expr_id "," expr_id "," expr_id ")"
    // size_spec: "<" expr [ "," expr ] [ "," ID ] [ ";" expr ] ">"
    //          | NOTHING
    // tree_subckt_spec: "<" expr ">"
    //                 | "<" STRING ">"

    fn size_spec<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], SizeSpec<'a>, E> {
        tuple((
            expr,
            ctrl(',').ignore_then(expr).opt(),
            ctrl(',').ignore_then(ident).opt(),
            ctrl(';').ignore_then(expr).opt(),
        ))
        .ang_braced()
        .parse(i)
    }

    fn expr_id_comma_3_tuple<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], (ExprId<'a>, ExprId<'a>, ExprId<'a>), E> {
        expr_id
            .then_ignore(ctrl(','))
            .then(expr_id)
            .then_ignore(ctrl(','))
            .then(expr_id)
            .parened()
            .map(|((a, b), c)| (a, b, c))
            .parse(i)
    }

    fn expr_id_comma_4_tuple<'a, E: EE<'a>>(
        i: &'a [u8],
    ) -> IResult<&'a [u8], (ExprId<'a>, ExprId<'a>, ExprId<'a>, ExprId<'a>), E> {
        expr_id
            .then_ignore(ctrl(','))
            .then(expr_id)
            .then_ignore(ctrl(','))
            .then(expr_id)
            .then_ignore(ctrl(','))
            .then(expr_id)
            .parened()
            .map(|(((a, b), c), d)| (a, b, c, d))
            .parse(i)
    }

    fn prs_item<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], PrsItem<'a>, E> {
        let tree_subckt_spec = alt((
            expr.ang_braced().map(TreeSubcktSpec::Expr),
            string.ang_braced().map(TreeSubcktSpec::Str),
        ));
        let pass_n = kw("passn").ignore_then_cut(size_spec).then(expr_id_comma_3_tuple);
        let pass_p = kw("passp").ignore_then_cut(size_spec).then(expr_id_comma_3_tuple);
        let transgate = kw("transgate").ignore_then_cut(size_spec).then(expr_id_comma_4_tuple);

        let macro_loop = ctrl('(')
            .ignore_then(tuple((
                none,
                ident,
                ctrl(':').ignore_then(expr_range),
                ctrl(':').ignore_then(none),
                prs_body_row().many1().terminated(ctrl(')')),
            )))
            .context("macro loop");

        let rule = prs_expr.then(arrow_kind).then(expr_id).then(dir);
        let at_rule = prs_expr.then(arrow_kind).then_ignore(ctrl('@')).then(ident).then(dir);

        // TODO add check for non-empty in follow-on pass
        let sub_block = ident.then(tree_subckt_spec.opt()).then(prs_body_row().many0().braced());

        alt((
            // These three start with a unique keyword at the begining, so it is easy to apply the "cut" operator afterwords
            pass_n.map(|(a, b)| PrsItem::PassN(a, b)),
            pass_p.map(|(a, b)| PrsItem::PassP(a, b)),
            transgate.map(|(a, b)| PrsItem::TransGate(a, b)),
            // The other four are harder to tell apart. Both the macro-loop and the rule/at_rule can being with a paren.
            // Both rule/at_rule and sub-block being with a identifier.
            macro_loop.map(PrsItem::MacroRule),
            rule.map(|(((a, b), c), d)| PrsItem::Rule(a, b, c, d)),
            at_rule.map(|(((a, b), c), d)| PrsItem::AtRule(a, b, c, d)),
            sub_block.map(|((a, b), c)| PrsItem::SubBlock(a, b, c)),
        ))
        .parse(i)
    }

    fn prs_body_row<'a, E: EE<'a>>() -> impl Parser<&'a [u8], (Option<AttrList<'a>>, PrsItem<'a>), E> {
        attr_list.opt().then(prs_item)
    }

    // lang_prs: "prs" [ supply_spec ] [ "*" ] "{" [ prs_body ] "}"
    pub fn lang_prs<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangPrs<'a>, E> {
        kw("prs")
            .ignore_then(supply_spec.opt())
            .then(ctrl('*').p().opt())
            .then(prs_body_row().many0().braced())
            .map(|((a, b), c)| (a, b.is_some(), c))
            .parse(i)
    }
}

mod lang_spec {
    use super::*;

    pub mod ast {
        use super::*;

        #[derive(Debug, Clone, Copy)]
        pub enum TimingType {
            LtLt,
            Lt,
            Arrow,
        }

        #[derive(Debug)]
        pub enum SpecItem<'a> {
            Normal(Ident<'a>, Vec<ExprId<'a>>),
            Timing(
                Option<(ExprId<'a>, Option<Dir>)>,
                bool,
                ExprId<'a>,
                bool,
                Option<Dir>,
                TimingType,
                Option<Expr<'a>>,
                ExprId<'a>,
                bool,
                Option<Dir>,
            ),
        }

        #[derive(Debug)]
        pub struct SpecBody<'a> {
            pub requires_clause: Option<Vec<SpecItem<'a>>>,
            pub ensures_clause: Option<Vec<SpecItem<'a>>>,
            pub generic_clause: Vec<SpecItem<'a>>,
        }

        pub type LangSpec<'a> = SpecBody<'a>;
    }

    use ast::*;

    // spec_body: [ requires_clause ] [ ensures_clause ] [ generic_clause ]
    // requires_clause: "requires" "{" base_spec_body "}"
    // ensures_clause: "ensures" "{" base_spec_body "}"
    // generic_clause: base_spec_body
    // base_spec_body: {t-rec}
    //                spec_body_item base_spec_body
    //               | spec_body_item
    // spec_body_item: {excl}
    //                ID "(" { expr_id "," }* ")"
    //               | "timing" [ expr_id [ dir ] ":" ] [ "?" ]
    //                   expr_id [ "*" ] [ dir ]
    //                   timing_type [ "[" expr "]" ]
    //                   expr_id [ "*" ] [ dir ]
    // timing_type: {excl}
    //             "<<"
    //            | "<"
    //            | "->"
    fn spec_item<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], SpecItem<'a>, E> {
        let timing_type = alt((
            ctrl2('<', '<').p().value(TimingType::LtLt),
            ctrl('<').p().value(TimingType::Lt),
            ctrl2('-', '>').p().value(TimingType::Arrow),
        ));
        let normal_item = ident
            .then(expr_id.list1_sep_by(ctrl(',')).parened())
            .map(|(a, b)| SpecItem::Normal(a, b));
        let timing_item_body = tuple((
            opt(expr_id.then(dir.opt()).then_ignore(ctrl(':'))),
            ctrl('?').p().opt().map(|v| v.is_some()),
            expr_id,
            ctrl('*').p().opt().map(|v| v.is_some()),
            dir.opt(),
            timing_type,
            expr.bracketed().opt(),
            expr_id,
            ctrl('*').p().opt().map(|v| v.is_some()),
            dir.opt(),
        ));
        let timing_item = kw("timing")
            .ignore_then_cut(timing_item_body)
            .map(|(a, b, c, d, e, f, g, h, i, j)| SpecItem::Timing(a, b, c, d, e, f, g, h, i, j));
        normal_item.or(timing_item).parse(i)
    }

    // lang_spec: "spec" "{" spec_body "}"
    pub fn lang_spec<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangSpec<'a>, E> {
        let rc = kw("requires").ignore_then_cut(spec_item.many0().braced());
        let ec = kw("ensures").ignore_then_cut(spec_item.many0().braced());

        let braced_spec_body = ctrl('{')
            .ignore_then(rc.opt().then(ec.opt()).then(spec_item.many0().term_by(ctrl('}'))))
            .map(|((rc, ec), gc)| SpecBody {
                requires_clause: rc,
                ensures_clause: ec,
                generic_clause: gc,
            });

        kw("spec")
            .ignore_then_cut(braced_spec_body)
            .context("spec block")
            .parse(i)
    }
}

mod lang_dataflow {
    use super::*;

    pub mod ast {
        use super::*;

        #[derive(Debug)]
        pub enum ExprIdOrStar<'a> {
            ExprId(ExprId<'a>),
            Star,
        }

        #[derive(Debug)]
        pub enum ExprIdOrStarOrBar<'a> {
            ExprId(ExprId<'a>),
            Star,
            Bar,
        }

        #[derive(Debug, Clone, Copy)]
        pub enum BracketedOrParened {
            Bracketed,
            Parened,
        }

        #[derive(Debug)]
        pub enum DataflowItem<'a> {
            BracketedOrParenedFlow(
                Expr<'a>,
                Option<(BracketedOrParened, Expr<'a>, Option<Expr<'a>>)>,
                ExprId<'a>,
            ),
            BracedFlow(ExprIdOrStarOrBar<'a>, Vec<ExprIdOrStar<'a>>, Vec<ExprIdOrStar<'a>>),
            Cluster(Vec<Self>),
            Sink(ExprId<'a>),
        }

        pub type LangDataflow<'a> = (
            Option<(Ident<'a>, Vec<(Vec<ExprId<'a>>, Vec<ExprId<'a>>)>)>,
            Vec<DataflowItem<'a>>,
        );
    }

    use ast::*;

    // lang_dataflow: "dataflow" "{" [ dataflow_ordering ] { dataflow_items ";" }* "}"
    // dataflow_ordering: ID "{" { order_list ";" }* "}"
    // order_list: { expr_id "," }* "<" { expr_id "," }*
    // dataflow_items: expr "->" [ "[" expr [ "," expr ] "]" ] expr_id
    //               | expr "->" [ "(" expr [ "," expr ] ")" ] expr_id
    //               | "{" expr_id_or_star_or_bar "}" { expr_id_or_star "," }* "->" { expr_id_or_star "," }*
    //               | "dataflow_cluster" "{" { dataflow_items ";" }* "}"
    //               | expr_id "->" "*"
    // expr_id_or_star: {excl}
    //                 expr_id
    //                | "*"
    // expr_id_or_star_or_bar: {excl}
    //                        expr_id_or_star
    //                       | "|"
    fn expr_id_or_star<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ExprIdOrStar<'a>, E> {
        alt((
            map(expr_id, ExprIdOrStar::ExprId),
            map(ctrl('*'), |_| ExprIdOrStar::Star),
        ))(i)
    }

    fn dataflow_item<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], DataflowItem<'a>, E> {
        let expr_id_or_star_or_bar = alt((
            map(expr_id, ExprIdOrStarOrBar::ExprId),
            map(ctrl('*'), |_| ExprIdOrStarOrBar::Star),
            map(ctrl('|'), |_| ExprIdOrStarOrBar::Bar),
        ));

        let bracketed_inset = expr
            .then(ctrl(',').ignore_then(expr).opt())
            .bracketed()
            .map(|(a, b)| (BracketedOrParened::Bracketed, a, b));
        let parened_inset = expr
            .then(ctrl(',').ignore_then(expr).opt())
            .parened()
            .map(|(a, b)| (BracketedOrParened::Parened, a, b));
        let parened_or_boxed_flow = expr
            .then_ignore(ctrl2('-', '>'))
            .then(bracketed_inset.or(parened_inset).opt())
            .then(expr_id)
            .map(|((a, b), c)| DataflowItem::BracketedOrParenedFlow(a, b, c));
        let braced_flow = expr_id_or_star_or_bar
            .braced()
            .then(expr_id_or_star.list1_sep_by(ctrl(',')).term_by(ctrl2('-', '>')))
            .then(expr_id_or_star.list1_sep_by(ctrl(',')).p())
            .map(|((a, b), c)| DataflowItem::BracedFlow(a, b, c));
        let cluster = map(
            preceded(kw("dataflow_cluster"), dataflow_item.list1_sep_by(ctrl(';')).braced()),
            DataflowItem::Cluster,
        );
        let sink = map(
            terminated(expr_id, preceded(ctrl2('-', '>'), ctrl('*'))),
            DataflowItem::Sink,
        );

        alt((parened_or_boxed_flow, braced_flow, cluster, sink))(i)
    }

    fn order_list<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], (Vec<ExprId<'a>>, Vec<ExprId<'a>>), E> {
        expr_id
            .list1_sep_by(ctrl(','))
            .term_by(ctrl('<'))
            .then(expr_id.list1_sep_by(ctrl(',')).p())
            .parse(i)
    }

    pub fn lang_dataflow<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangDataflow<'a>, E> {
        let dataflow_ordering = pair(ident, order_list.list1_sep_by(ctrl(';')).braced());

        kw("dataflow")
            .ignore_then(ctrl('{'))
            .ignore_then(dataflow_ordering.opt())
            .then(dataflow_item.list1_sep_by(ctrl(';')).term_by(ctrl('}')))
            .parse(i)
    }
}

mod lang_initialize {

    use super::{
        lang_chp_hse::{ast::HseItemList, hse_body},
        *,
    };

    pub mod ast {
        use super::*;

        pub type LangInitialize<'a> = Vec<(Ident<'a>, HseItemList<'a>)>;
    }

    use ast::*;

    // lang_initialize: "Initialize" "{" { action_items ";" }* "}"
    // action_items: ID "{" hse_body "}"
    pub fn lang_initialize<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangInitialize<'a>, E> {
        let action_item = ident.then(hse_body.braced());
        kw("initialize")
            .ignore_then_cut(action_item.list1_sep_by(ctrl(';')).braced())
            .context("initialize block")
            .parse(i)
    }
}

mod lang_sizing {
    use super::*;
    pub mod ast {
        use super::*;
        pub type DirectivePart<'a> = (Dir, Expr<'a>, Option<Ident<'a>>, Option<Expr<'a>>);

        #[derive(Debug)]
        pub enum SizeDirective<'a> {
            Directive(ExprId<'a>, (DirectivePart<'a>, Option<DirectivePart<'a>>)),
            MacroLoop(MacroLoop<'a, Ctrl<'a>, (), Vec<SizeDirective<'a>>>),
        }
        pub type LangSizing<'a> = (Vec<Option<(Ident<'a>, Expr<'a>)>>, Vec<SizeDirective<'a>>);
    }
    use ast::*;

    // size_setup: ID "<-" expr
    //           | NOTHING
    // size_directive: {excl}
    //                expr_id "{" dir expr [ "," ID ] [ "," expr ] [ ";" dir expr [ "," ID ] [ "," expr ] ] "}"
    //               | "(" ";" ID ":" !noreal expr [ ".." expr ] ":" { size_directive ";" }* ")"
    // size_body: { size_setup ";" }* { size_directive ";" }*
    // lang_size: "sizing" "{" [ size_body ] "}"

    fn directive_part<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], DirectivePart<'a>, E> {
        dir.then(expr)
            .then(ctrl(',').ignore_then(ident).opt())
            .then(ctrl(',').ignore_then(expr).opt())
            .map(|(((a, b), c), d)| (a, b, c, d))
            .parse(i)
    }

    fn size_directive<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], SizeDirective<'a>, E> {
        let directive = expr_id
            .then(
                directive_part
                    .then(ctrl(';').ignore_then(directive_part).opt())
                    .braced(),
            )
            .map(|(a, b)| SizeDirective::Directive(a, b));
        let macro_loop = ctrl('(')
            .precedes(tuple((
                ctrl(';'),
                ident,
                preceded(ctrl(':'), expr_range),
                preceded(ctrl(':'), none),
                size_directive.list1_sep_by(ctrl(';')).terminated(ctrl(')')),
            )))
            .context("macro loop")
            .map(SizeDirective::MacroLoop);
        directive.or(macro_loop).parse(i)
    }

    pub fn lang_sizing<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangSizing<'a>, E> {
        let size_setups = ident
            .and(ctrl2('<', '-').precedes(expr))
            .opt()
            .list1_sep_by(ctrl(';'))
            .p();
        let size_directives = size_directive.list1_sep_by(ctrl(';'));

        kw("sizing")
            .ignore_then_cut(ctrl('{').ignore_then(size_setups.then(size_directives.term_by(ctrl('}')))))
            .parse(i)
    }
}

pub mod ast {
    pub use super::lang_chp_hse::ast::*;
    pub use super::lang_dataflow::ast::*;
    pub use super::lang_initialize::ast::*;
    pub use super::lang_prs::ast::*;
    pub use super::lang_sizing::ast::*;
    pub use super::lang_spec::ast::*;
}
pub use lang_chp_hse::{hse_body, lang_chp, lang_hse};
pub use lang_dataflow::lang_dataflow;
pub use lang_initialize::lang_initialize;
pub use lang_prs::lang_prs;
pub use lang_sizing::lang_sizing;
pub use lang_spec::lang_spec;
