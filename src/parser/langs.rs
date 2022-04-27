use super::basic::{ast::*, *};
use crate::parser::utils::{MyParserExt, ParserExt2, ET};
use nom::{
    branch::alt,
    combinator::{not, peek},
    sequence::tuple,
    IResult, Parser,
};
use nom_supreme::parser_ext::ParserExt;

mod lang_chp_hse {
    use super::*;
    use crate::parser::utils::{SepList1, Unterm};

    pub mod ast {
        use super::*;

        #[derive(Debug)]
        pub struct LangChp<'a>(
            pub Kw<'a>,
            pub Option<SupplySpec<'a>>,
            pub CtrlLBrace<'a>,
            pub Option<ChpItemList<'a>>,
            pub CtrlRBrace<'a>,
        );

        #[derive(Debug, Copy, Clone)]
        pub enum SemiOrComma {
            Semi,
            Comma,
        }

        #[derive(Debug)]
        pub struct ChpItem<'a>(pub Option<(Ident<'a>, CtrlColon<'a>)>, pub ChpStmt<'a>);
        #[derive(Debug)]
        pub struct ChpItemList<'a>(pub SepList1<SepList1<ChpItem<'a>, CtrlComma<'a>>, CtrlSemi<'a>>);

        #[derive(Debug)]
        pub struct AssignStmt<'a>(pub ExprId<'a>, pub Ctrl<'a> /* := */, pub Expr<'a>);
        #[derive(Debug)]
        pub struct AssignBoolDirStmt<'a>(pub ExprId<'a>, pub (Dir, Ctrl<'a>));

        #[derive(Debug)]
        pub struct ChpMacroLoop<'a>(
            pub CtrlLParen<'a>,
            pub (SemiOrComma, Ctrl<'a>),
            pub Ident<'a>,
            pub CtrlColon<'a>,
            pub ExprRange<'a>,
            pub CtrlColon<'a>,
            pub ChpItemList<'a>,
            pub CtrlRParen<'a>,
        );

        #[derive(Debug)]
        pub enum ChpStmt<'a> {
            Assign(AssignStmt<'a>),
            AssignBoolDir(AssignBoolDirStmt<'a>),
            SendStmt(SendStmt<'a>),
            RecvStmt(RecvStmt<'a>),
            Skip(Kw<'a>),
            ParenedBody(CtrlLParen<'a>, ChpItemList<'a>, CtrlRParen<'a>),
            FuncCall(
                Ident<'a>,
                CtrlLParen<'a>,
                SepList1<ExprOrStr<'a>, CtrlComma<'a>>,
                CtrlRParen<'a>,
            ),
            DottedCall(
                BaseId<'a>,
                CtrlDot<'a>,
                Ident<'a>,
                CtrlLParen<'a>,
                SepList1<Expr<'a>, CtrlComma<'a>>,
                CtrlRParen<'a>,
            ),
            BracketedStmt(ChpBracketedStmt<'a>),
            MacroLoop(ChpMacroLoop<'a>),
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
        #[derive(Debug, Copy, Clone)]
        pub enum RecvTypeCast<'a> {
            AsInt(Kw<'a>, CtrlLParen<'a>, Ident<'a>, CtrlRParen<'a>),
            AsBool(Kw<'a>, CtrlLParen<'a>, Ident<'a>, CtrlRParen<'a>),
            Ident(Ident<'a>),
        }
        #[derive(Debug)]
        pub struct SendStmt<'a>(
            pub ExprId<'a>,
            pub (SendType, Ctrl<'a>),
            pub Option<Expr<'a>>,
            pub Option<((RecvType, Ctrl<'a>), RecvTypeCast<'a>)>,
        );
        #[derive(Debug)]
        pub struct RecvStmt<'a>(
            pub ExprId<'a>,
            pub (RecvType, Ctrl<'a>),
            pub Option<RecvTypeCast<'a>>,
            pub Option<((SendType, Ctrl<'a>), Expr<'a>)>,
        );

        #[derive(Debug)]
        pub enum GuardedCmd<'a> {
            Expr(Expr<'a>, CtrlLArrow<'a>, ChpItemList<'a>),
            Else(Kw<'a>, CtrlLArrow<'a>, ChpItemList<'a>),
            Macro(
                CtrlLParen<'a>,
                Ctrl<'a>, /* [] */
                Ident<'a>,
                CtrlColon<'a>,
                ExprRange<'a>,
                CtrlColon<'a>,
                Expr<'a>,
                CtrlLArrow<'a>,
                ChpItemList<'a>,
                CtrlRParen<'a>,
            ),
        }

        #[derive(Debug)]
        pub enum ChpBracketedStmt<'a> {
            DetermSelect(
                CtrlLBracket<'a>,
                SepList1<GuardedCmd<'a>, Ctrl<'a> /* [] */>,
                CtrlRBracket<'a>,
            ),
            NonDetermSelect(
                Ctrl<'a>, /* [| */
                SepList1<GuardedCmd<'a>, Ctrl<'a> /* [] */>,
                Ctrl<'a>, /* |] */
            ),
            Wait(CtrlLBracket<'a>, Expr<'a>, CtrlRBracket<'a>),
            DoLoop(
                Ctrl<'a>, /* *[ */
                ChpItemList<'a>,
                Option<(Ctrl<'a> /* <- */, Expr<'a>)>,
                CtrlRBracket<'a>,
            ),
            WhileLoop(
                Ctrl<'a>, /* *[ */
                SepList1<GuardedCmd<'a>, Ctrl<'a> /* [] */>,
                CtrlRBracket<'a>,
            ),
        }

        // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
        #[derive(Debug)]
        pub struct LabeledHseBody<'a>(
            pub Ident<'a>,
            pub CtrlColon<'a>,
            pub HseItemList<'a>,
            pub CtrlColon<'a>,
            pub Ident<'a>,
        );

        #[derive(Debug)]
        pub enum HseBodies<'a> {
            Body(HseItemList<'a>),
            Labeled(SepList1<LabeledHseBody<'a>, CtrlSemi<'a>>),
        }
        #[derive(Debug)]
        pub struct HseItemList<'a>(pub ChpItemList<'a>);
        #[derive(Debug)]
        pub struct LangHse<'a>(
            pub Kw<'a>,
            pub Option<SupplySpec<'a>>,
            pub CtrlLBrace<'a>,
            pub Option<HseBodies<'a>>,
            pub CtrlRBrace<'a>,
        );
    }

    use ast::*;

    fn send_type(i: &[u8]) -> IResult<&[u8], (SendType, Ctrl), ET> {
        alt((
            ctrl2('!', '+').map(|v| (SendType::Plus, v)),
            ctrl2('!', '-').map(|v| (SendType::Minus, v)),
            ctrl('!').map(|v| (SendType::Normal, v)),
        ))
        .parse(i)
    }
    fn recv_type(i: &[u8]) -> IResult<&[u8], (RecvType, Ctrl), ET> {
        alt((
            ctrl2('?', '+').map(|v| (RecvType::Plus, v)),
            ctrl2('?', '-').map(|v| (RecvType::Minus, v)),
            ctrl('?').map(|v| (RecvType::Normal, v)),
        ))
        .parse(i)
    }

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
    pub fn recv_type_cast(i: &[u8]) -> IResult<&[u8], RecvTypeCast, ET> {
        alt((
            kw("bool")
                .then_cut(ident.parened())
                .map(|(a, (b, c, d))| RecvTypeCast::AsBool(a, b, c, d)),
            kw("int")
                .then_cut(ident.parened())
                .map(|(a, (b, c, d))| RecvTypeCast::AsInt(a, b, c, d)),
            ident.map(RecvTypeCast::Ident),
        ))
        .parse(i)
    }
    pub fn chp_stmt(i: &[u8]) -> IResult<&[u8], ChpStmt, ET> {
        // These all begin with a unique token
        // skip -> `skip`   `[` or `*[`
        let skip_stmt = kw("skip").context("skip_stmt").map(ChpStmt::Skip);
        let chp_select_or_loop_stmt = chp_select_or_loop_stmt
            .context("chp_select_or_loop_stmt")
            .map(ChpStmt::BracketedStmt);

        // The next two both begin with a `(`. If it is followed by a `;` or a `,`, then it is a
        // macro loop. Otherwise, it is a parened body
        let semi_or_comma = alt((
            ctrl(';').map(|v| (SemiOrComma::Semi, v)),
            ctrl(',').map(|v| (SemiOrComma::Comma, v)),
        ));
        let macro_loop = ctrl('(')
            .then(semi_or_comma)
            .then_cut(
                ident
                    .then(ctrl(':'))
                    .then(expr_range)
                    .then(ctrl(':'))
                    .then(chp_item_list1().term_by(ctrl(')'))),
            )
            .context("macro_loop")
            .map(|((a, b), ((((c, d), e), f), (g, h)))| ChpMacroLoop(a, b, c, d, e, f, ChpItemList(g), h))
            .map(ChpStmt::MacroLoop);

        let parened_body = ctrl('(')
            .then_cut(chp_item_list1().term_by(ctrl(')')).verify(|v| v.0.items.len() >= 2))
            .map(|(a, (b, c))| ChpStmt::ParenedBody(a, ChpItemList(b), c))
            .context("parened body");

        // This goes `ident (` at the start
        let func_call = ident
            .then(ctrl('('))
            .then_cut(expr_or_str.list1_sep_by(ctrl(',')).term_by(ctrl(')')))
            .map(|((a, b), (c, d))| ChpStmt::FuncCall(a, b, c, d))
            .context("func_call");

        // otherwise, it should begin with a expr_id. Therefore, to get good error messages, we will
        // parse an expr_id, and then attempt to parse the rest of one of these expressions
        pub enum ChpStmtAfterExprId<'a> {
            Assign(Ctrl<'a> /* := */, Expr<'a>),
            AssignBoolDir((Dir, Ctrl<'a>)),
            SendStmt(
                (SendType, Ctrl<'a>),
                Option<Expr<'a>>,
                Option<((RecvType, Ctrl<'a>), RecvTypeCast<'a>)>,
            ),
            RecvStmt(
                (RecvType, Ctrl<'a>),
                Option<RecvTypeCast<'a>>,
                Option<((SendType, Ctrl<'a>), Expr<'a>)>,
            ),
        }

        let send_stmt_after_ei = send_type
            .then_cut(expr.opt().then_opt(recv_type.then_cut(recv_type_cast)))
            .map(|(a, (b, c))| ChpStmtAfterExprId::SendStmt(a, b, c));

        let recv_stmt_after_ei = recv_type
            .then_cut(recv_type_cast.opt().then_opt(send_type.then_cut(expr)))
            .map(|(a, (b, c))| ChpStmtAfterExprId::RecvStmt(a, b, c));

        let assign_stmt_after_ei = ctrl2(':', '=')
            .then(expr)
            .map(|(a, b)| ChpStmtAfterExprId::Assign(a, b));
        let assign_bool_dir_stmt_after_ei = dir.map(ChpStmtAfterExprId::AssignBoolDir);

        let chp_stmt_after_ei = expr_id
            .then(alt((
                uncut(send_stmt_after_ei),
                uncut(recv_stmt_after_ei),
                uncut(assign_stmt_after_ei),
                uncut(assign_bool_dir_stmt_after_ei),
            )))
            .context("chp stmt after expr id")
            .map(|(a, b)| match b {
                ChpStmtAfterExprId::Assign(b, c) => ChpStmt::Assign(AssignStmt(a, b, c)),
                ChpStmtAfterExprId::AssignBoolDir(b) => ChpStmt::AssignBoolDir(AssignBoolDirStmt(a, b)),
                ChpStmtAfterExprId::SendStmt(b, c, d) => ChpStmt::SendStmt(SendStmt(a, b, c, d)),
                ChpStmtAfterExprId::RecvStmt(b, c, d) => ChpStmt::RecvStmt(RecvStmt(a, b, c, d)),
            });

        // The other four begin with a base_id (meaning an ident followed by zero or mode bracketed ranges)
        // e.g. ident[a][b..c][d+e..f{g..h}].
        // If it starts with an identifier followed by a `[` or a `.`, it is a dotted_call
        let dotted_call = base_id
            .then(ctrl('.'))
            .then(ident)
            .then(expr.list1_sep_by(ctrl(',')).parened())
            .context("dotted_call")
            .map(|(((a, b), c), (d, e, f))| ChpStmt::DottedCall(a, b, c, d, e, f));

        // Now put all the parsers together in a big alt block
        alt((
            // this is just a unique keyword
            skip_stmt,
            // these begin with unique control sequences ('[', '*[', and '(') respectively
            chp_select_or_loop_stmt,
            macro_loop, // goes before parened body
            parened_body,
            // this start with an `ident (`
            func_call,
            // This rest of them start with an base_id or expr_id
            uncut(dotted_call),
            uncut(chp_stmt_after_ei),
        ))
        .context("chp stmt")
        .parse(i)
    }
    pub fn chp_item(i: &[u8]) -> IResult<&[u8], ChpItem, ET> {
        let is_labeled_detector = peek(ident.then(ctrl(':')).then(not(ctrl('='))));
        let label = ident.then(ctrl(':'));
        alt((
            // If it begins `ident : NOT(=)` then it is a labeled statement
            is_labeled_detector
                .ignore_then_cut(label.then(chp_stmt))
                .map(|(label, stmt)| ChpItem(Some(label), stmt)),
            // Otherwise it is an unlabeled statement
            chp_stmt.map(|stmt| ChpItem(None, stmt)),
        ))
        .parse(i)
    }

    fn chp_item_list1<'a>() -> Unterm<impl Parser<&'a [u8], SepList1<ChpItem<'a>, CtrlComma<'a>>, ET<'a>>> {
        let chp_comma_list = chp_item.list1_sep_by(ctrl(',')).p();
        chp_comma_list.list1_sep_by(ctrl(';'))
    }

    // assign_stmt: expr_id ":=" expr
    //            | expr_id dir
    //            ;

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

    fn guarded_cmd(i: &[u8]) -> IResult<&[u8], GuardedCmd, ET> {
        let macro_branch = ctrl('(')
            .then(ctrl2('[', ']'))
            .then_cut(tuple((
                ident,
                ctrl(':'),
                expr_range,
                ctrl(':'),
                expr,
                ctrl2('-', '>'),
                chp_item_list1().term_by(ctrl(')')),
            )))
            .context("macro loop")
            .map(|((a, b), (c, d, e, f, g, h, (i, j)))| GuardedCmd::Macro(a, b, c, d, e, f, g, h, ChpItemList(i), j));
        let else_branch = kw("else")
            .then_cut(ctrl2('-', '>').then(chp_item_list1().p()))
            .map(|(a, (b, c))| GuardedCmd::Else(a, b, ChpItemList(c)));
        let normal_branch = expr
            .then(ctrl2('-', '>'))
            .then(chp_item_list1().p())
            .map(|((a, b), c)| GuardedCmd::Expr(a, b, ChpItemList(c)));

        alt((macro_branch, else_branch, normal_branch)).parse(i)
    }

    // TODO the is probably a better way of doing this (for the purposes of extracting errors?)
    fn chp_select_or_loop_stmt(i: &[u8]) -> IResult<&[u8], ChpBracketedStmt, ET> {
        #[derive(Copy, Clone)]
        enum Tp {
            DetermSelect,
            NonDetermSelect,
            WhileLoop,
        }
        // First try extracting a loop/select statement with guards
        let open = alt((
            ctrl2('*', '[').map(|v| (Tp::WhileLoop, v)),
            ctrl2('[', '|').map(|v| (Tp::NonDetermSelect, v)),
            ctrl('[').map(|v| (Tp::DetermSelect, v)),
        ));

        let loop_ = open
            .then_cut(
                guarded_cmd
                    .list1_sep_by(ctrl2('[', ']'))
                    .term_by(ctrl2('|', ']').or(ctrl(']'))),
            )
            .map(|((tp, a), (b, c))| match tp {
                // TODO handle closing symbol correctly
                Tp::DetermSelect => ChpBracketedStmt::DetermSelect(a, b, c),
                Tp::NonDetermSelect => ChpBracketedStmt::NonDetermSelect(a, b, c),
                Tp::WhileLoop => ChpBracketedStmt::WhileLoop(a, b, c),
            })
            .context("while loop or select");

        // the try parsing a wait statement
        let wait = ctrl('[')
            .then_cut(expr.then(ctrl(']')))
            .map(|(a, (b, c))| ChpBracketedStmt::Wait(a, b, c))
            .context("wait");

        // the try parsing a do-loop
        let do_loop = ctrl2('*', '[')
            .then_cut(
                chp_item_list1()
                    .term_by_recog_alt2(ctrl2('<', '-'), ctrl(']'))
                    .then_opt(ctrl2('<', '-').then_cut(expr))
                    .then(ctrl(']')),
            )
            .map(|(a, ((b, c), d))| ChpBracketedStmt::DoLoop(a, ChpItemList(b), c, d))
            .context("do loop");

        // at most one of these should succeed. If more do, raise an error! Otherwise, return the successful one
        alt((uncut(wait), uncut(do_loop), uncut(loop_))).parse(i)
    }

    // hse_bodies: hse_body
    //           | labelled_hse_bodies
    // labelled_hse_bodies: { label_hse_fragment ";" }*
    // label_hse_fragment: ID ":" hse_body ":" ID
    // hse_body: { hse_comma_item ";" }*
    // hse_comma_item: { hse_body_item "," }*
    // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
    pub fn hse_body(i: &[u8]) -> IResult<&[u8], HseItemList, ET> {
        chp_item_list1().p().map(ChpItemList).map(HseItemList).parse(i)
    }

    fn hse_bodies(i: &[u8]) -> IResult<&[u8], HseBodies, ET> {
        let labeled_body = ident
            .then(ctrl(':'))
            .then(hse_body)
            .then(ctrl(':'))
            .then(ident)
            .map(|((((a, b), c), d), e)| LabeledHseBody(a, b, c, d, e));
        let labeled_bodies = labeled_body.list1_sep_by(ctrl(';')).p().map(HseBodies::Labeled);
        hse_body.map(HseBodies::Body).or(labeled_bodies).parse(i)
    }

    // lang_chp: "chp" [ supply_spec ] "{" [ chp_body ] "}"
    // lang_hse: "hse" [ supply_spec ] "{" [ hse_bodies ] "}"
    // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
    pub fn lang_hse(i: &[u8]) -> IResult<&[u8], LangHse, ET> {
        kw("hse")
            .then_opt(supply_spec)
            .then(hse_bodies.opt().braced())
            .map(|((a, b), (c, d, e))| LangHse(a, b, c, d, e))
            .context("hse block")
            .parse(i)
    }

    pub fn lang_chp(i: &[u8]) -> IResult<&[u8], LangChp, ET> {
        let block = supply_spec.opt().then(chp_item_list1().opt().braced());
        kw("chp")
            .then_cut(block)
            .map(|(a, (b, (c, d, e)))| LangChp(a, b, c, d.map(ChpItemList), e))
            .context("chp block")
            .parse(i)
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

        #[derive(Debug)]
        pub struct SizeSpec<'a>(
            pub CtrlLAngBrace<'a>,
            pub Expr<'a>,
            pub Option<(CtrlComma<'a>, Expr<'a>, Option<(CtrlComma<'a>, Ident<'a>)>)>,
            pub Option<(CtrlSemi<'a>, Expr<'a>)>,
            pub CtrlRAngBrace<'a>,
        );

        #[derive(Debug)]
        pub enum TreeSubcktSpec<'a> {
            Expr(Expr<'a>),
            Str(StrTok<'a>),
        }

        #[derive(Debug)]
        pub struct PrsMacroLoop<'a>(
            pub CtrlLParen<'a>,
            pub Ident<'a>,
            pub CtrlColon<'a>,
            pub ExprRange<'a>,
            pub CtrlColon<'a>,
            pub PrsBody<'a>,
            pub CtrlRParen<'a>,
        );

        #[derive(Debug)]
        pub enum PassNPKind {
            N,
            P,
        }
        #[derive(Debug)]
        pub enum PrsItem<'a> {
            Rule(PrsExpr<'a>, (ArrowKind, Ctrl<'a>), ExprId<'a>, (Dir, Ctrl<'a>)),
            AtRule(
                PrsExpr<'a>,
                (ArrowKind, Ctrl<'a>),
                CtrlAtSign<'a>,
                Ident<'a>,
                (Dir, Ctrl<'a>),
            ),
            SubBlock(
                Ident<'a>,
                Option<(CtrlLAngBrace<'a>, TreeSubcktSpec<'a>, CtrlRAngBrace<'a>)>,
                CtrlLBrace<'a>,
                PrsBody<'a>,
                CtrlRBrace<'a>,
            ),
            MacroRule(PrsMacroLoop<'a>),
            Pass(
                (PassNPKind, Kw<'a>),
                SizeSpec<'a>,
                CtrlLParen<'a>,
                ExprId<'a>,
                CtrlComma<'a>,
                ExprId<'a>,
                CtrlComma<'a>,
                ExprId<'a>,
                CtrlRParen<'a>,
            ),
            TransGate(
                Kw<'a>,
                SizeSpec<'a>,
                CtrlLParen<'a>,
                ExprId<'a>,
                CtrlComma<'a>,
                ExprId<'a>,
                CtrlComma<'a>,
                ExprId<'a>,
                CtrlComma<'a>,
                ExprId<'a>,
                CtrlRParen<'a>,
            ),
        }

        #[derive(Debug)]
        pub struct PrsBodyRow<'a>(pub Option<BracketedAttrList<'a>>, pub PrsItem<'a>);
        #[derive(Debug)]
        pub struct PrsBody<'a>(pub Vec<PrsBodyRow<'a>>);
        #[derive(Debug)]
        pub struct LangPrs<'a>(
            pub Kw<'a>,
            pub Option<SupplySpec<'a>>,
            pub Option<CtrlStar<'a>>,
            pub CtrlLBrace<'a>,
            pub PrsBody<'a>,
            pub CtrlRBrace<'a>,
        );
    }

    use ast::*;

    // arrow: {excl}
    //       "->"
    //      | "=>"
    //      | "#>"
    fn arrow_kind(i: &[u8]) -> IResult<&[u8], (ArrowKind, Ctrl), ET> {
        alt((
            ctrl2('-', '>').map(|v| (ArrowKind::Minus, v)),
            ctrl2('=', '>').map(|v| (ArrowKind::Equals, v)),
            ctrl2('#', '>').map(|v| (ArrowKind::Hash, v)),
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

    fn size_spec(i: &[u8]) -> IResult<&[u8], SizeSpec, ET> {
        ctrl('<')
            .then_cut(
                expr.then_opt(ctrl(',').then_cut(expr.then_opt(ctrl(',').then_cut(ident))))
                    .then_opt(ctrl(';').then_cut(expr)),
            )
            .then(ctrl('>'))
            .map(|((a, ((b, c), d)), e)| SizeSpec(a, b, c.map(|(x, (y, z))| (x, y, z)), d, e))
            .parse(i)
    }

    fn expr_id_comma_3_tuple(
        i: &[u8],
    ) -> IResult<&[u8], (CtrlLParen, ExprId, CtrlComma, ExprId, CtrlComma, ExprId, CtrlRParen), ET> {
        expr_id
            .then(ctrl(','))
            .then(expr_id)
            .then(ctrl(','))
            .then(expr_id)
            .parened()
            .map(|(a, ((((b, c), d), e), f), g)| (a, b, c, d, e, f, g))
            .parse(i)
    }

    fn expr_id_comma_4_tuple(
        i: &[u8],
    ) -> IResult<
        &[u8],
        (
            CtrlLParen,
            ExprId,
            CtrlComma,
            ExprId,
            CtrlComma,
            ExprId,
            CtrlComma,
            ExprId,
            CtrlRParen,
        ),
        ET,
    > {
        expr_id
            .then(ctrl(','))
            .then(expr_id)
            .then(ctrl(','))
            .then(expr_id)
            .then(ctrl(','))
            .then(expr_id)
            .parened()
            .map(|(a, ((((((b, c), d), e), f), g), h), i)| (a, b, c, d, e, f, g, h, i))
            .parse(i)
    }

    fn prs_item(i: &[u8]) -> IResult<&[u8], PrsItem, ET> {
        let pass = alt((
            kw("passn").map(|v| (PassNPKind::N, v)),
            kw("passn").map(|v| (PassNPKind::P, v)),
        ))
        .then_cut(size_spec.then(expr_id_comma_3_tuple))
        .map(|(a, (b, (c, d, e, f, g, h, i)))| PrsItem::Pass(a, b, c, d, e, f, g, h, i));
        let transgate = kw("transgate")
            .then_cut(size_spec.then(expr_id_comma_4_tuple))
            .map(|(a, (b, (c, d, e, f, g, h, i, j, k)))| PrsItem::TransGate(a, b, c, d, e, f, g, h, i, j, k));

        let macro_loop = ctrl('(')
            .then(tuple((
                ident,
                ctrl(':'),
                expr_range,
                ctrl(':'),
                prs_body_row().many1().term_by(ctrl(')')).map(|(a, b)| (PrsBody(a), b)),
            )))
            .context("macro loop")
            .map(|(a, (b, c, d, e, (f, g)))| PrsMacroLoop(a, b, c, d, e, f, g))
            .map(PrsItem::MacroRule);

        let rule = prs_expr
            .then(arrow_kind)
            .then(expr_id)
            .then(dir)
            .map(|(((a, b), c), d)| PrsItem::Rule(a, b, c, d));
        let at_rule = prs_expr
            .then(arrow_kind)
            .then(ctrl('@'))
            .then(ident)
            .then(dir)
            .map(|((((a, b), c), d), e)| PrsItem::AtRule(a, b, c, d, e));

        // TODO add check for non-empty in follow-on pass
        let tree_subckt_spec = alt((
            string.map(TreeSubcktSpec::Str),
            expr_no_gt.cut().map(TreeSubcktSpec::Expr),
        ));
        let sub_block = ident
            .then_opt(
                ctrl('<')
                    .then_cut(tree_subckt_spec.then(ctrl('>')))
                    .map(|(x, (y, z))| (x, y, z)),
            )
            .then(prs_body_row().many0().braced())
            .map(|((a, b), (c, d, e))| PrsItem::SubBlock(a, b, c, PrsBody(d), e));

        alt((
            // These three start with a unique keyword at the begining, so it is easy to apply the "cut" operator afterwords
            pass,
            transgate,
            // The other four are harder to tell apart. Both the macro-loop and the rule/at_rule can being with a paren.
            // Both rule/at_rule and sub-block being with a identifier.

            // TODO apply a "longest of" here
            uncut(macro_loop),
            uncut(rule),
            uncut(at_rule),
            uncut(sub_block),
        ))
        .parse(i)
    }

    #[inline]
    fn prs_body_row<'a>() -> impl Parser<&'a [u8], PrsBodyRow<'a>, ET<'a>> {
        attr_list.opt().then(prs_item).map(|(a, b)| PrsBodyRow(a, b))
    }

    // lang_prs: "prs" [ supply_spec ] [ "*" ] "{" [ prs_body ] "}"
    pub fn lang_prs(i: &[u8]) -> IResult<&[u8], LangPrs, ET> {
        kw("prs")
            .then_opt(supply_spec)
            .then_opt(ctrl('*'))
            .then(prs_body_row().many0().braced().map(|(x, y, z)| (x, PrsBody(y), z)))
            .map(|(((a, b), c), (d, e, f))| LangPrs(a, b, c, d, e, f))
            .parse(i)
    }
}

mod lang_spec {
    use super::*;

    pub mod ast {
        use super::*;
        use crate::parser::utils::SepList1;

        #[derive(Debug, Clone, Copy)]
        pub enum TimingType {
            LtLt,
            Lt,
            Arrow,
        }

        #[derive(Debug)]
        pub struct TimingBodyClause<'a>(pub ExprId<'a>, pub Option<CtrlStar<'a>>, pub Option<(Dir, Ctrl<'a>)>);
        #[derive(Debug)]
        pub struct TimingBody<'a>(
            pub TimingBodyClause<'a>,
            pub Option<CtrlQMark<'a>>,
            pub Option<(CtrlColon<'a>, TimingBodyClause<'a>)>,
            pub (TimingType, Ctrl<'a>),
            pub Option<(CtrlLBracket<'a>, Expr<'a>, CtrlRBracket<'a>)>,
            pub TimingBodyClause<'a>,
        );

        #[derive(Debug)]
        pub enum SpecItem<'a> {
            Normal(
                Ident<'a>,
                CtrlLParen<'a>,
                SepList1<ExprId<'a>, CtrlComma<'a>>,
                CtrlRParen<'a>,
            ),
            Timing(Kw<'a>, TimingBody<'a>),
        }

        #[derive(Debug)]
        pub struct SpecBody<'a> {
            pub lbrace: CtrlLBrace<'a>,
            pub requires_clause: Option<(Kw<'a>, (CtrlLBrace<'a>, Vec<SpecItem<'a>>, CtrlRBrace<'a>))>,
            pub ensures_clause: Option<(Kw<'a>, (CtrlLBrace<'a>, Vec<SpecItem<'a>>, CtrlRBrace<'a>))>,
            pub generic_clause: Vec<SpecItem<'a>>,
            pub rbrace: CtrlRBrace<'a>,
        }

        #[derive(Debug)]
        pub struct LangSpec<'a>(pub Kw<'a>, pub SpecBody<'a>);
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
    fn timing_clause(i: &[u8]) -> IResult<&[u8], TimingBodyClause, ET> {
        expr_id
            .then_opt(ctrl('*'))
            .then_opt(dir)
            .map(|((a, b), c)| TimingBodyClause(a, b, c))
            .parse(i)
    }
    fn timing_item_body(i: &[u8]) -> IResult<&[u8], TimingBody, ET> {
        // "timing" [ expr_id [ dir ] ":" ] [ "?" ] expr_id [ "*" ] [ dir ]
        //            timing_type [ "[" expr "]" ] expr_id [ "*" ] [ dir ]
        // can be expanded to
        // "timing" expr_id [ "*" ] [ dir ]
        // AND THEN EITHER
        // [ ":"  [ "?" ]  expr_id [ "*" ] [ dir ]] OR
        // [ "?" ]
        // AND THEN
        // timing_type [ "[" expr "]" ]
        // expr_id [ "*" ] [ dir ]
        let timing_type = alt((
            ctrl2('<', '<').map(|v| (TimingType::LtLt, v)),
            ctrl('<').map(|v| (TimingType::Lt, v)),
            ctrl2('-', '>').map(|v| (TimingType::Arrow, v)),
        ));
        let opt_second_cluase = alt((
            ctrl(':')
                .then_cut(ctrl('?').opt().then(timing_clause))
                .map(|(a, (b, c))| (b, Some((a, c)))),
            ctrl('?').opt().map(|b| (b, None)),
        ));
        timing_clause
            .then(opt_second_cluase)
            .then(timing_type)
            .then_opt(ctrl('[').then_cut(expr.then(ctrl(']'))).map(|(x, (y, z))| (x, y, z)))
            .then(timing_clause)
            .map(|((((a, (b, c)), d), e), f)| TimingBody(a, b, c, d, e, f))
            .parse(i)
    }
    fn spec_item(i: &[u8]) -> IResult<&[u8], SpecItem, ET> {
        let normal_item = ident
            .then(expr_id.list1_sep_by(ctrl(',')).parened())
            .map(|(a, (b, c, d))| SpecItem::Normal(a, b, c, d));

        let timing_item = kw("timing")
            .then_cut(timing_item_body)
            .map(|(a, b)| SpecItem::Timing(a, b));
        normal_item.or(timing_item).parse(i)
    }

    // lang_spec: "spec" "{" spec_body "}"
    pub fn lang_spec(i: &[u8]) -> IResult<&[u8], LangSpec, ET> {
        let rc = kw("requires").then_cut(spec_item.many0().braced());
        let ec = kw("ensures").then_cut(spec_item.many0().braced());

        let braced_spec_body = ctrl('{')
            .then(rc.opt().then(ec.opt()).then(spec_item.many0().term_by(ctrl('}'))))
            .map(|(lbrace, ((rc, ec), (gc, rbrace)))| SpecBody {
                lbrace,
                requires_clause: rc,
                ensures_clause: ec,
                generic_clause: gc,
                rbrace,
            });

        kw("spec")
            .then_cut(braced_spec_body)
            .map(|(a, b)| LangSpec(a, b))
            .context("spec block")
            .parse(i)
    }
}

mod lang_dataflow {
    use super::*;

    pub mod ast {
        use super::*;
        use crate::parser::utils::SepList1;

        #[derive(Debug)]
        pub enum ExprIdOrStar<'a> {
            ExprId(ExprId<'a>),
            Star(CtrlStar<'a>),
        }

        #[derive(Debug)]
        pub enum ExprIdOrStarOrBar<'a> {
            ExprId(ExprId<'a>),
            Star(CtrlStar<'a>),
            Bar(CtrlVBar<'a>),
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
                CtrlLArrow<'a>,
                Option<(
                    BracketedOrParened,
                    Ctrl<'a>, /* [ OR ( */
                    Expr<'a>,
                    Option<(Ctrl<'a>, Expr<'a>)>,
                    Ctrl<'a>, /* ] OR ) */
                )>,
                ExprId<'a>,
            ),
            BracedFlow(
                CtrlLBrace<'a>,
                ExprIdOrStarOrBar<'a>,
                CtrlRBrace<'a>,
                SepList1<ExprIdOrStar<'a>, CtrlComma<'a>>,
                CtrlLArrow<'a>,
                SepList1<ExprIdOrStar<'a>, CtrlComma<'a>>,
            ),
            Cluster(Kw<'a>, CtrlLBrace<'a>, SepList1<Self, CtrlSemi<'a>>, CtrlRBrace<'a>),
            Sink(ExprId<'a>, CtrlLArrow<'a>, CtrlStar<'a>),
        }

        #[derive(Debug)]
        pub struct DataflowOrdering<'a>(
            pub Ident<'a>,
            pub CtrlLBrace<'a>,
            pub  SepList1<
                (
                    SepList1<ExprId<'a>, CtrlComma<'a>>,
                    Ctrl<'a>, /* < */
                    SepList1<ExprId<'a>, CtrlComma<'a>>,
                ),
                CtrlSemi<'a>,
            >,
            pub CtrlRBrace<'a>,
        );
        #[derive(Debug)]
        pub struct LangDataflow<'a>(
            pub Kw<'a>,
            pub CtrlLBrace<'a>,
            pub Option<DataflowOrdering<'a>>,
            pub SepList1<DataflowItem<'a>, CtrlSemi<'a>>,
            pub CtrlRBrace<'a>,
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
    fn expr_id_or_star(i: &[u8]) -> IResult<&[u8], ExprIdOrStar, ET> {
        alt((
            ctrl('*').map(ExprIdOrStar::Star),
            expr_id.cut().map(ExprIdOrStar::ExprId),
        ))
        .parse(i)
    }

    fn dataflow_item(i: &[u8]) -> IResult<&[u8], DataflowItem, ET> {
        let expr_id_or_star_or_bar = alt((
            expr_id.map(ExprIdOrStarOrBar::ExprId),
            ctrl('*').map(ExprIdOrStarOrBar::Star),
            ctrl('|').map(ExprIdOrStarOrBar::Bar),
        ));

        let cluster = kw("dataflow_cluster")
            .then_cut(dataflow_item.list1_sep_by(ctrl(';')).braced())
            .map(|(a, (b, c, d))| DataflowItem::Cluster(a, b, c, d));

        let bracketed_inset = ctrl('[')
            .then_cut(expr.then_opt(ctrl(',').then_cut(expr)).then(ctrl(']')))
            .map(|(a, ((b, c), d))| (BracketedOrParened::Bracketed, a, b, c, d));
        let parened_inset = ctrl('(')
            .then_cut(expr.then_opt(ctrl(',').then_cut(expr)).then(ctrl(')')))
            .map(|(a, ((b, c), d))| (BracketedOrParened::Parened, a, b, c, d));
        let parened_or_boxed_flow = expr
            .then(ctrl2('-', '>'))
            .then_opt(bracketed_inset.or(parened_inset))
            .then(expr_id)
            .map(|(((a, b), c), d)| DataflowItem::BracketedOrParenedFlow(a, b, c, d));
        let braced_flow = expr_id_or_star_or_bar
            .braced()
            .then(expr_id_or_star.list1_sep_by(ctrl(',')).term_by(ctrl2('-', '>')))
            .then(expr_id_or_star.list1_sep_by(ctrl(',')).p())
            .map(|(((a, b, c), (d, e)), f)| DataflowItem::BracedFlow(a, b, c, d, e, f));

        let sink = expr_id
            .then(ctrl2('-', '>'))
            .then(ctrl('*'))
            .map(|((a, b), c)| DataflowItem::Sink(a, b, c));

        alt((
            // cluster starts with a kw("dataflow_cluster")
            cluster,
            // The other ones start with either an expr or expr_id
            uncut(parened_or_boxed_flow),
            uncut(braced_flow),
            uncut(sink),
        ))
        .parse(i)
    }

    pub fn lang_dataflow(i: &[u8]) -> IResult<&[u8], LangDataflow, ET> {
        let order_list = expr_id
            .list1_sep_by(ctrl(','))
            .term_by(ctrl('<'))
            .then(expr_id.list1_sep_by(ctrl(',')).p())
            .map(|((a, b), c)| (a, b, c));

        // since no dataflow item starts `IDENT {`
        let dataflow_ordering = ident
            .then(ctrl('{'))
            .then_cut(order_list.list1_sep_by(ctrl(';')).term_by(ctrl('}')))
            .map(|((a, b), (c, d))| DataflowOrdering(a, b, c, d));

        kw("dataflow")
            .then_cut(
                ctrl('{')
                    .then_opt(dataflow_ordering)
                    .then(dataflow_item.list1_sep_by(ctrl(';')).term_by(ctrl('}'))),
            )
            .map(|(a, ((b, c), (d, e)))| LangDataflow(a, b, c, d, e))
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
        use crate::parser::utils::SepList1;

        #[derive(Debug)]
        pub struct ActionItem<'a>(
            pub Ident<'a>,
            pub CtrlLBrace<'a>,
            pub HseItemList<'a>,
            pub CtrlRBrace<'a>,
        );
        #[derive(Debug)]
        pub struct LangInitialize<'a>(
            pub Kw<'a>,
            pub CtrlLBrace<'a>,
            pub SepList1<ActionItem<'a>, CtrlSemi<'a>>,
            pub CtrlRBrace<'a>,
        );
    }

    use ast::*;

    // lang_initialize: "Initialize" "{" { action_items ";" }* "}"
    // action_items: ID "{" hse_body "}"
    pub fn lang_initialize(i: &[u8]) -> IResult<&[u8], LangInitialize, ET> {
        let action_item = ident
            .then(hse_body.braced())
            .map(|(a, (b, c, d))| ActionItem(a, b, c, d));
        kw("initialize")
            .then_cut(action_item.list1_sep_by(ctrl(';')).braced())
            .map(|(a, (b, c, d))| LangInitialize(a, b, c, d))
            .context("initialize block")
            .parse(i)
    }
}

mod lang_sizing {
    use super::*;
    pub mod ast {
        use super::*;
        use crate::parser::utils::SepList1;
        #[derive(Debug)]
        pub struct DirectivePart<'a>(
            pub (Dir, Ctrl<'a>),
            pub Expr<'a>,
            pub Option<(CtrlComma<'a>, (Ident<'a>, Option<(CtrlComma<'a>, Expr<'a>)>))>,
        );

        #[derive(Debug)]
        pub struct SizeDirectiveMacroLoop<'a>(
            pub CtrlLParen<'a>,
            pub CtrlSemi<'a>,
            pub Ident<'a>,
            pub CtrlColon<'a>,
            pub ExprRange<'a>,
            pub CtrlColon<'a>,
            pub SepList1<SizeDirective<'a>, CtrlSemi<'a>>,
            pub CtrlRParen<'a>,
        );
        #[derive(Debug)]
        pub enum SizeDirective<'a> {
            Item(
                ExprId<'a>,
                CtrlLBrace<'a>,
                DirectivePart<'a>,
                Option<(CtrlSemi<'a>, DirectivePart<'a>)>,
                CtrlRBrace<'a>,
            ),
            MacroLoop(SizeDirectiveMacroLoop<'a>),
        }
        #[derive(Debug)]
        pub struct LangSizing<'a>(
            pub Kw<'a>,
            pub CtrlLBrace<'a>,
            pub SepList1<Option<(Ident<'a>, Expr<'a>)>, CtrlSemi<'a>>,
            pub SepList1<SizeDirective<'a>, CtrlSemi<'a>>,
            pub CtrlRBrace<'a>,
        );
    }
    use ast::*;

    // size_setup: ID "<-" expr
    //           | NOTHING
    // size_directive: {excl}
    //                expr_id "{" dir expr [ "," ID ] [ "," expr ] [ ";" dir expr [ "," ID ] [ "," expr ] ] "}"
    //               | "(" ";" ID ":" !noreal expr [ ".." expr ] ":" { size_directive ";" }* ")"
    // size_body: { size_setup ";" }* { size_directive ";" }*
    // lang_size: "sizing" "{" [ size_body ] "}"

    fn directive_part(i: &[u8]) -> IResult<&[u8], DirectivePart, ET> {
        dir.then(expr)
            .then_opt(ctrl(',').then_cut(ident.then_opt(ctrl(',').then_cut(expr))))
            .map(|((a, b), c)| DirectivePart(a, b, c))
            .parse(i)
    }

    fn size_directive(i: &[u8]) -> IResult<&[u8], SizeDirective, ET> {
        let directive = expr_id
            .then(directive_part.then_opt(ctrl(';').then_cut(directive_part)).braced())
            .map(|(a, (b, (c, d), e))| SizeDirective::Item(a, b, c, d, e));
        let macro_loop = ctrl('(')
            .then_cut(tuple((
                ctrl(';'),
                ident,
                ctrl(':').then(expr_range).then(ctrl(':')),
                size_directive.list1_sep_by(ctrl(';')).term_by(ctrl(')')),
            )))
            .map(|(a, (b, c, ((d, e), f), (g, h)))| SizeDirectiveMacroLoop(a, b, c, d, e, f, g, h))
            .map(SizeDirective::MacroLoop)
            .context("macro loop");
        macro_loop.or(directive).parse(i)
    }

    pub fn lang_sizing(i: &[u8]) -> IResult<&[u8], LangSizing, ET> {
        let one_size_setup = ident.then_cut(ctrl2('<', '-').precedes(expr));
        let size_setups = one_size_setup.opt().list1_sep_by(ctrl(';')).p();
        let size_directives = size_directive.list1_sep_by(ctrl(';'));

        kw("sizing")
            .then_cut(ctrl('{').then(size_setups).then(size_directives.term_by(ctrl('}'))))
            .map(|(a, ((b, c), (d, e)))| LangSizing(a, b, c, d, e))
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
