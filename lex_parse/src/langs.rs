use nom::{
    branch::alt,
    combinator::{not, peek},
    IResult,
    Parser, sequence::tuple,
};
use nom_supreme::parser_ext::ParserExt;

pub use lang_chp_hse::{hse_body, lang_chp, lang_hse};
pub use lang_dataflow::lang_dataflow;
pub use lang_initialize::lang_initialize;
pub use lang_prs::lang_prs;
pub use lang_sizing::lang_sizing;
pub use lang_spec::lang_spec;

use crate::utils::{ET, MyParserExt, ParserExt2};

use super::basic::{*, ast::*};

mod lang_chp_hse {
    use ast::*;

    use crate::utils::{SepList1, Unterm};

    use super::*;

    pub mod ast {
        use super::*;

        #[derive(Debug)]
        pub struct LangChp(
            pub Kw,
            pub Option<SupplySpec>,
            pub CtrlLBrace,
            pub Option<ChpItemList>,
            pub CtrlRBrace,
        );

        #[derive(Debug, Copy, Clone)]
        pub enum SemiOrComma {
            Semi,
            Comma,
        }

        #[derive(Debug)]
        pub struct ChpItem(pub Option<(Ident, CtrlColon)>, pub ChpStmt);

        #[derive(Debug)]
        pub struct ChpItemList(pub SepList1<SepList1<ChpItem, CtrlComma>, CtrlSemi>);

        #[derive(Debug)]
        pub struct AssignStmt(pub ExprId, pub Ctrl /* := */, pub Expr);

        #[derive(Debug)]
        pub struct AssignBoolDirStmt(pub ExprId, pub (Dir, Ctrl));

        #[derive(Debug)]
        pub enum ChpStmt {
            Assign(AssignStmt),
            AssignBoolDir(AssignBoolDirStmt),
            SendStmt(SendStmt),
            RecvStmt(RecvStmt),
            Skip(Kw),
            ParenedBody(CtrlLParen, ChpItemList, CtrlRParen),
            FuncCall(Ident, CtrlLParen, SepList1<ExprOrStr, CtrlComma>, CtrlRParen),
            DottedCall(
                BaseId,
                CtrlDot,
                Ident,
                CtrlLParen,
                SepList1<Expr, CtrlComma>,
                CtrlRParen,
            ),
            BracketedStmt(ChpBracketedStmt),
            MacroLoop(MacroLoop<(SemiOrComma, Ctrl), ChpItemList>),
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
        pub enum RecvTypeCast {
            AsInt(Kw, CtrlLParen, ExprId, CtrlRParen),
            AsBool(Kw, CtrlLParen, ExprId, CtrlRParen),
            Ident(ExprId),
        }

        #[derive(Debug)]
        pub struct SendStmt(
            pub ExprId,
            pub (SendType, Ctrl),
            pub Option<Expr>,
            pub Option<((RecvType, Ctrl), RecvTypeCast)>,
        );

        #[derive(Debug)]
        pub struct RecvStmt(
            pub ExprId,
            pub (RecvType, Ctrl),
            pub Option<RecvTypeCast>,
            pub Option<((SendType, Ctrl), Expr)>,
        );

        #[derive(Debug)]
        pub enum GuardedCmd {
            Expr(Expr, CtrlLArrow, ChpItemList),
            Else(Kw, CtrlLArrow, ChpItemList),
            Macro(
                CtrlLParen,
                Ctrl, /* [] */
                Ident,
                CtrlColon,
                ExprRange,
                CtrlColon,
                Expr,
                CtrlLArrow,
                ChpItemList,
                CtrlRParen,
            ),
        }

        #[derive(Debug)]
        pub enum ChpBracketedStmt {
            DetermSelect(CtrlLBracket, SepList1<GuardedCmd, Ctrl /* [] */>, CtrlRBracket),
            NonDetermSelect(
                Ctrl, /* [| */
                SepList1<GuardedCmd, Ctrl /* [] */>,
                Ctrl, /* |] */
            ),
            Wait(CtrlLBracket, Expr, CtrlRBracket),
            DoLoop(
                Ctrl, /* *[ */
                ChpItemList,
                Option<(Ctrl /* <- */, Expr)>,
                CtrlRBracket,
            ),
            WhileLoop(Ctrl /* *[ */, SepList1<GuardedCmd, Ctrl /* [] */>, CtrlRBracket),
        }

        // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
        #[derive(Debug)]
        pub struct LabeledHseBody(pub Ident, pub CtrlColon, pub HseItemList, pub CtrlColon, pub Ident);

        #[derive(Debug)]
        pub enum HseBodies {
            Body(HseItemList),
            Labeled(SepList1<LabeledHseBody, CtrlSemi>),
        }

        #[derive(Debug)]
        pub struct HseItemList(pub ChpItemList);

        #[derive(Debug)]
        pub struct LangHse(
            pub Kw,
            pub Option<SupplySpec>,
            pub CtrlLBrace,
            pub Option<HseBodies>,
            pub CtrlRBrace,
        );
    }

    fn send_type<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], (SendType, Ctrl), E> {
        alt((
            ctrl2('!', '+').p().map(|v| (SendType::Plus, v)),
            ctrl2('!', '-').p().map(|v| (SendType::Minus, v)),
            ctrl('!').p().map(|v| (SendType::Normal, v)),
        ))
            .parse(i)
    }

    fn recv_type<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], (RecvType, Ctrl), E> {
        alt((
            ctrl2('?', '+').p().map(|v| (RecvType::Plus, v)),
            ctrl2('?', '-').p().map(|v| (RecvType::Minus, v)),
            ctrl('?').p().map(|v| (RecvType::Normal, v)),
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
    pub fn recv_type_cast<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], RecvTypeCast, E> {
        alt((
            kw("bool")
                .then_cut(expr_id.parened())
                .map(|(a, (b, c, d))| RecvTypeCast::AsBool(a, b, c, d)),
            kw("int")
                .then_cut(expr_id.parened())
                .map(|(a, (b, c, d))| RecvTypeCast::AsInt(a, b, c, d)),
            expr_id.map(RecvTypeCast::Ident),
        ))
            .parse(i)
    }

    pub fn chp_stmt<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ChpStmt, E> {
        // These all begin with a unique token
        // skip -> `skip`   `[` or `*[`
        let skip_stmt = kw("skip").p().context("skip_stmt").map(ChpStmt::Skip);
        let chp_select_or_loop_stmt = chp_select_or_loop_stmt
            .context("chp_select_or_loop_stmt")
            .map(ChpStmt::BracketedStmt);

        // The next two both begin with a `(`. If it is followed by a `;` or a `,`, then it is a
        // macro loop. Otherwise, it is a parened body
        let semi_or_comma = alt((
            ctrl(';').p().map(|v| (SemiOrComma::Semi, v)),
            ctrl(',').p().map(|v| (SemiOrComma::Comma, v)),
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
            .map(|((a, b), ((((c, d), e), f), (g, h)))| MacroLoop(a, b, c, d, e, f, ChpItemList(g), h))
            .map(ChpStmt::MacroLoop);

        let parened_body = ctrl('(')
            .then_cut(chp_item_list1().term_by(ctrl(')')))
            // .verify(|v| v.0.items.len() >= 2 || (v.0.items.len() >= 1 && v.0.items[0].items.len() >= 2)))
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
        pub enum ChpStmtAfterExprId {
            Assign(Ctrl /* := */, Expr),
            AssignBoolDir((Dir, Ctrl)),
            SendStmt((SendType, Ctrl), Option<Expr>, Option<((RecvType, Ctrl), RecvTypeCast)>),
            RecvStmt((RecvType, Ctrl), Option<RecvTypeCast>, Option<((SendType, Ctrl), Expr)>),
        }

        let send_stmt_after_ei = send_type
            .then_cut(expr_no_qmark.opt().then_opt(recv_type.then_cut(recv_type_cast)))
            .map(|(a, (b, c))| ChpStmtAfterExprId::SendStmt(a, b, c))
            .context("send stmt");

        let recv_stmt_after_ei = recv_type
            .then_cut(recv_type_cast.opt().then_opt(send_type.then_cut(expr)))
            .map(|(a, (b, c))| ChpStmtAfterExprId::RecvStmt(a, b, c))
            .context("recv stmt");

        let assign_stmt_after_ei = ctrl2(':', '=')
            .then(expr)
            .map(|(a, b)| ChpStmtAfterExprId::Assign(a, b))
            .context("assign stmt");
        let assign_bool_dir_stmt_after_ei = dir.map(ChpStmtAfterExprId::AssignBoolDir).context("assign dir");

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

    pub fn chp_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ChpItem, E> {
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

    fn chp_item_list1<'a, E: ET<'a>>() -> Unterm<impl Parser<&'a [u8], SepList1<ChpItem, CtrlComma>, E>> {
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

    fn guarded_cmd<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], GuardedCmd, E> {
        let macro_branch = ctrl('(').p()
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
            .context("macro loop branch")
            .map(|((a, b), (c, d, e, f, g, h, (i, j)))| GuardedCmd::Macro(a, b, c, d, e, f, g, h, ChpItemList(i), j));
        let else_branch = kw("else")
            .then_cut(ctrl2('-', '>').then(chp_item_list1().p()))
            .context("else branch")
            .map(|(a, (b, c))| GuardedCmd::Else(a, b, ChpItemList(c)));
        let normal_branch = expr
            .then(ctrl2('-', '>'))
            .then(chp_item_list1().p())
            .context("normal branch")
            .map(|((a, b), c)| GuardedCmd::Expr(a, b, ChpItemList(c)));

        alt((macro_branch, else_branch, normal_branch)).parse(i)
    }

    // TODO the is probably a better way of doing this (for the purposes of extracting errors?)
    fn chp_select_or_loop_stmt<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ChpBracketedStmt, E> {
        #[derive(Copy, Clone)]
        enum Tp {
            DetermSelect,
            NonDetermSelect,
            WhileLoop,
        }
        // First try extracting a loop/select statement with guards
        let open = alt((
            ctrl2('*', '[').p().map(|v| (Tp::WhileLoop, v)),
            ctrl2('[', '|').p().map(|v| (Tp::NonDetermSelect, v)),
            ctrl('[').p().map(|v| (Tp::DetermSelect, v)),
        ));

        let loop_ = open
            .then_cut(
                guarded_cmd
                    .list1_sep_by(ctrl2('[', ']'))
                    .term_by(ctrl2('|', ']').p().or(ctrl(']'))),
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
        let result = alt((uncut(wait), uncut(do_loop), uncut(loop_))).parse(i);

        // Now we do the grossest part of parsing. In a do-loop, the guard is preceded by `<-`. However,
        // if someone writes `*[ A:=B<-C ]`, it is honestly not clear whether they mean `*[ A:= B; skip <- C ]`
        // or if they mean `*[ A:= (B  <- C) ]`. The parses we have written a greedy, and so will parse
        // it as `*[ A:= (B  <- C) ]`.
        //
        // Unfortunately, the Act library currently parses this as `*[ A:= B; skip <- C ]`, so we must
        // also. To do some, we do a bit of a hack. If we parsed a do-loop above with no guard, we
        // parse the longest guard expression we can among the tokens used in the final expression.
        // Then, we parse all the remaining tokens as a do-loop again (without the final terminator)
        // and return that.

        match result {
            Ok((ii, bracketed)) => {
                match bracketed {
                    ChpBracketedStmt::DoLoop(open_bracket, items, oexpr, close_bracket) => {
                        match oexpr {
                            Some((_, _)) => Ok((ii, ChpBracketedStmt::DoLoop(open_bracket, items, oexpr, close_bracket))),
                            None => {
                                // then we dont have a guard, but maybe we should
                                assert_ne!(items.0.items.len(), 0);
                                let ctrl_before = match items.0.items.last().unwrap().seps.last() {
                                    Some(last_comma) => last_comma,
                                    None => match items.0.seps.last() {
                                        Some(last_semi) => last_semi,
                                        None => &open_bracket
                                    }
                                };
                                // this should never be at the end of a successfully parsed file (need e.g. the closing brace on the chp block)
                                assert_ne!(ii.len(), 0);
                                let start_offset = ctrl_before.ft_ptr_last();
                                let end_offset = FTPtr::of_ptr(&ii[0]);
                                // last_ii holds the input in the final chp statement
                                let last_ii = &i[i.len() - ii.len() - FTPtr::offset_between(&start_offset, &end_offset) + 1..i.len() - ii.len() - 1];

                                // then, scan through the list for any potential guard symbols, and try
                                // parsing around each, starting with the left-most one
                                let split = (0..last_ii.len() - 1).into_iter().filter(|i|
                                    ctrl2('<', '-').p::<crate::error::ErrorIgnorer<'a>>().parse(&last_ii[*i..i + 2]).is_ok()
                                ).find_map(|i| {
                                    let r1 = chp_item::<'a, crate::error::ErrorIgnorer<'a>>.complete().parse(&last_ii[0..i]);
                                    let r2 = expr::<'a, crate::error::ErrorIgnorer<'a>>.complete().parse(&last_ii[i + 2..last_ii.len()]);
                                    if r1.is_ok() && r2.is_ok() {
                                        let c = ctrl2('<', '-').p::<'a, crate::error::ErrorIgnorer<'a>>().parse(&last_ii[i..i + 2]);
                                        Some((r1.unwrap().1, c.unwrap().1, r2.unwrap().1))
                                    } else {
                                        None
                                    }
                                });

                                match split {
                                    None => Ok((ii, ChpBracketedStmt::DoLoop(open_bracket, items, oexpr, close_bracket))),
                                    Some((item, arrow, expr)) => {
                                        let mut items = items;
                                        let ln = items.0.items.len() - 1;
                                        let last_comma_list = &mut items.0.items[ln];
                                        let ln = last_comma_list.items.len() - 1;
                                        last_comma_list.items[ln] = item;

                                        Ok((ii, ChpBracketedStmt::DoLoop(open_bracket, items, Some((arrow, expr)), close_bracket)))
                                    }
                                }
                            }
                        }
                    }
                    e => Ok((ii, e))
                }
            }
            e => e
        }
    }

    // hse_bodies: hse_body
    //           | labelled_hse_bodies
    // labelled_hse_bodies: { label_hse_fragment ";" }*
    // label_hse_fragment: ID ":" hse_body ":" ID
    // hse_body: { hse_comma_item ";" }*
    // hse_comma_item: { hse_body_item "," }*
    // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
    pub fn hse_body<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], HseItemList, E> {
        chp_item_list1()
            .p()
            .map(ChpItemList)
            .map(HseItemList)
            .context("hse body")
            .parse(i)
    }

    fn hse_bodies<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], HseBodies, E> {
        let labeled_body = ident
            .then(ctrl1_not_ctrl2(':', '='))
            .then_cut(hse_body.then(ctrl(':')).then(ident))
            .map(|((a, b), ((c, d), e))| LabeledHseBody(a, b, c, d, e))
            .context("labeled hse body");
        alt((
            labeled_body
                .list1_sep_by(ctrl(';'))
                .p()
                .map(HseBodies::Labeled)
                .context("labeled hse bodies"),
            hse_body.map(HseBodies::Body).context("normal hse body"),
        ))
            .parse(i)
    }

    // lang_chp: "chp" [ supply_spec ] "{" [ chp_body ] "}"
    // lang_hse: "hse" [ supply_spec ] "{" [ hse_bodies ] "}"
    // Instead, HSE should be parsed like chp, and then checked afterwords that it is in the correct subset of chp
    pub fn lang_hse<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangHse, E> {
        kw("hse")
            .then_cut(opt_supply_spec.then(ctrl('{')).then(alt((
                ctrl('}').p().map(|v| (None, v)),
                hse_bodies.then(ctrl('}')).cut().map(|(a, b)| (Some(a), b)),
            ))))
            .map(|(a, ((b, c), (d, e)))| LangHse(a, b, c, d, e))
            .context("hse block")
            .parse(i)
    }

    pub fn lang_chp<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangChp, E> {
        let block = opt_supply_spec.then(chp_item_list1().opt().braced());
        kw("chp")
            .then_cut(block)
            .map(|(a, (b, (c, d, e)))| LangChp(a, b, c, d.map(ChpItemList), e))
            .context("chp block")
            .parse(i)
    }
}

mod lang_prs {
    use ast::*;

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
        pub struct SizeSpec(
            pub CtrlLAngBrace,
            pub Expr,
            pub Option<(CtrlComma, Expr, Option<(CtrlComma, Ident)>)>,
            pub Option<(CtrlSemi, Expr)>,
            pub CtrlRAngBrace,
        );

        #[derive(Debug)]
        pub enum TreeSubcktSpec {
            Expr(Expr),
            Str(StrTok),
        }

        #[derive(Debug)]
        pub enum PassNPKind {
            N,
            P,
        }

        #[derive(Debug)]
        pub enum PrsItem {
            Rule(PrsExpr, (ArrowKind, Ctrl), ExprId, (Dir, Ctrl)),
            AtRule(PrsExpr, (ArrowKind, Ctrl), CtrlAtSign, Ident, (Dir, Ctrl)),
            SubBlock(
                Ident,
                Option<(CtrlLAngBrace, TreeSubcktSpec, CtrlRAngBrace)>,
                CtrlLBrace,
                PrsBody,
                CtrlRBrace,
            ),
            MacroLoop(MacroLoop<(), PrsBody>),
            Pass(
                (PassNPKind, Kw),
                Option<SizeSpec>,
                CtrlLParen,
                ExprId,
                CtrlComma,
                ExprId,
                CtrlComma,
                ExprId,
                CtrlRParen,
            ),
            TransGate(
                Kw,
                Option<SizeSpec>,
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
        }

        #[derive(Debug)]
        pub struct PrsBodyRow(pub Option<BracketedAttrList>, pub PrsItem);

        #[derive(Debug)]
        pub struct PrsBody(pub Vec<PrsBodyRow>);

        #[derive(Debug)]
        pub struct LangPrs(
            pub Kw,
            pub Option<SupplySpec>,
            pub Option<CtrlStar>,
            pub CtrlLBrace,
            pub PrsBody,
            pub CtrlRBrace,
        );
    }

    // arrow: {excl}
    //       "->"
    //      | "=>"
    //      | "#>"
    fn arrow_kind<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], (ArrowKind, Ctrl), E> {
        alt((
            ctrl2('-', '>').p().map(|v| (ArrowKind::Minus, v)),
            ctrl2('=', '>').p().map(|v| (ArrowKind::Equals, v)),
            ctrl2('#', '>').p().map(|v| (ArrowKind::Hash, v)),
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

    fn size_spec<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], SizeSpec, E> {
        ctrl('<')
            .then_cut(
                expr_no_gt
                    .then_opt(ctrl(',').then_cut(expr_no_gt.then_opt(ctrl(',').then_cut(ident))))
                    .then_opt(ctrl(';').then_cut(expr_no_gt)),
            )
            .then(ctrl('>'))
            .map(|((a, ((b, c), d)), e)| SizeSpec(a, b, c.map(|(x, (y, z))| (x, y, z)), d, e))
            .parse(i)
    }

    fn expr_id_comma_3_tuple<'a, E: ET<'a>>(
        i: &'a [u8],
    ) -> IResult<&[u8], (CtrlLParen, ExprId, CtrlComma, ExprId, CtrlComma, ExprId, CtrlRParen), E> {
        expr_id
            .then(ctrl(','))
            .then(expr_id)
            .then(ctrl(','))
            .then(expr_id)
            .parened()
            .map(|(a, ((((b, c), d), e), f), g)| (a, b, c, d, e, f, g))
            .parse(i)
    }

    fn expr_id_comma_4_tuple<'a, E: ET<'a>>(
        i: &'a [u8],
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
        E,
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

    fn prs_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], PrsItem, E> {
        let pass = alt((
            kw("passn").p().map(|v| (PassNPKind::N, v)),
            kw("passp").p().map(|v| (PassNPKind::P, v)),
        ))
            .then_cut(size_spec.opt().then(expr_id_comma_3_tuple))
            .map(|(a, (b, (c, d, e, f, g, h, i)))| PrsItem::Pass(a, b, c, d, e, f, g, h, i));
        let transgate = kw("transgate")
            .then_cut(size_spec.opt().then(expr_id_comma_4_tuple))
            .map(|(a, (b, (c, d, e, f, g, h, i, j, k)))| PrsItem::TransGate(a, b, c, d, e, f, g, h, i, j, k));

        let macro_loop = ctrl('(')
            .then(tuple((
                ident,
                ctrl(':'),
                expr_range,
                ctrl(':'),
                prs_body_row().many1().term_by(ctrl(')')).map(|(a, b)| (PrsBody(a), b)),
            )))
            .context("prs macro loop")
            .map(|(a, (b, c, d, e, (f, g)))| MacroLoop(a, (), b, c, d, e, f, g))
            .map(PrsItem::MacroLoop);

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
            pass.context("pass"),
            transgate.context("transgate"),
            // The other four are harder to tell apart. Both the macro-loop and the rule/at_rule can being with a paren.
            // Both rule/at_rule and sub-block being with a identifier.

            // TODO apply a "longest of" here
            uncut(macro_loop.context("macro loop")),
            uncut(rule.context("rule")),
            uncut(at_rule.context("at rule")),
            uncut(sub_block.context("subblock")),
        ))
            .parse(i)
    }

    #[inline]
    fn prs_body_row<'a, E: ET<'a>>() -> impl Parser<&'a [u8], PrsBodyRow, E> {
        cut_bracketed_attr_list
            .opt()
            .then(prs_item)
            .map(|(a, b)| PrsBodyRow(a, b))
            .context("prs body row")
    }

    // lang_prs: "prs" [ supply_spec ] [ "*" ] "{" [ prs_body ] "}"
    pub fn lang_prs<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangPrs, E> {
        kw("prs")
            .then(opt_supply_spec)
            .then_opt(ctrl('*'))
            .then(prs_body_row().many0().braced().map(|(x, y, z)| (x, PrsBody(y), z)))
            .map(|(((a, b), c), (d, e, f))| LangPrs(a, b, c, d, e, f))
            .context("prs block")
            .parse(i)
    }
}

mod lang_spec {
    use ast::*;

    use super::*;

    pub mod ast {
        use crate::utils::SepList1;

        use super::*;

        #[derive(Debug, Clone, Copy)]
        pub enum TimingType {
            LtLt,
            Lt,
            Arrow,
        }

        #[derive(Debug)]
        pub struct TimingBodyClause(pub ExprId, pub Option<CtrlStar>, pub Option<(Dir, Ctrl)>);

        #[derive(Debug)]
        pub struct TimingBody(
            pub TimingBodyClause,
            pub Option<CtrlQMark>,
            pub Option<(CtrlColon, TimingBodyClause)>,
            pub (TimingType, Ctrl),
            pub Option<(CtrlLBracket, Expr, CtrlRBracket)>,
            pub TimingBodyClause,
        );

        #[derive(Debug)]
        pub enum SpecItem {
            Normal(Ident, CtrlLParen, SepList1<ExprIdOrStar, CtrlComma>, CtrlRParen),
            Timing(Kw, TimingBody),
        }

        #[derive(Debug)]
        pub struct SpecBody {
            pub lbrace: CtrlLBrace,
            pub requires_clause: Option<(Kw, (CtrlLBrace, Vec<SpecItem>, CtrlRBrace))>,
            pub ensures_clause: Option<(Kw, (CtrlLBrace, Vec<SpecItem>, CtrlRBrace))>,
            pub generic_clause: Vec<SpecItem>,
            pub rbrace: CtrlRBrace,
        }

        #[derive(Debug)]
        pub struct LangSpec(pub Kw, pub SpecBody);
    }

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
    fn timing_clause<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], TimingBodyClause, E> {
        expr_id
            .then_opt(ctrl('*'))
            .then_opt(dir)
            .map(|((a, b), c)| TimingBodyClause(a, b, c))
            .parse(i)
    }

    fn timing_item_body<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], TimingBody, E> {
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
            ctrl2('<', '<').p().map(|v| (TimingType::LtLt, v)),
            ctrl('<').p().map(|v| (TimingType::Lt, v)),
            ctrl2('-', '>').p().map(|v| (TimingType::Arrow, v)),
        ));
        let opt_second_cluase = alt((
            ctrl(':')
                .then_cut(ctrl('?').p().opt().then(timing_clause))
                .map(|(a, (b, c))| (b, Some((a, c)))),
            ctrl('?').p().opt().map(|b| (b, None)),
        ));
        timing_clause
            .then(opt_second_cluase)
            .then(timing_type)
            .then_opt(ctrl('[').then_cut(expr.then(ctrl(']'))).map(|(x, (y, z))| (x, y, z)))
            .then(timing_clause)
            .map(|((((a, (b, c)), d), e), f)| TimingBody(a, b, c, d, e, f))
            .parse(i)
    }

    fn spec_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], SpecItem, E> {
        // TODO is this right?
        let normal_item = ident
            .then(expr_id_or_star.list1_sep_by(ctrl(',')).parened())
            .map(|(a, (b, c, d))| SpecItem::Normal(a, b, c, d));

        let timing_item = kw("timing")
            .then_cut(timing_item_body)
            .map(|(a, b)| SpecItem::Timing(a, b));
        normal_item.or(timing_item).parse(i)
    }

    // lang_spec: "spec" "{" spec_body "}"
    pub fn lang_spec<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangSpec, E> {
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
    use ast::*;

    use super::*;

    pub mod ast {
        use crate::utils::SepList1;

        use super::*;

        #[derive(Debug, Clone, Copy)]
        pub enum BracketedOrParened {
            Bracketed,
            Parened,
        }

        #[derive(Debug)]
        pub enum DataflowItem {
            BracketedOrParenedFlow(
                Expr,
                CtrlLArrow,
                Option<(
                    BracketedOrParened,
                    Ctrl, /* [ OR ( */
                    Expr,
                    Option<(Ctrl, Expr)>,
                    Ctrl, /* ] OR ) */
                )>,
                ExprId,
            ),
            BracedFlow(
                CtrlLBrace,
                ExprIdOrStarOrBar,
                CtrlRBrace,
                SepList1<ExprIdOrStar, CtrlComma>,
                CtrlLArrow,
                SepList1<ExprIdOrStar, CtrlComma>,
            ),
            Cluster(Kw, CtrlLBrace, SepList1<Self, CtrlSemi>, CtrlRBrace),
            Sink(ExprId, CtrlLArrow, CtrlStar),
        }

        #[derive(Debug)]
        pub struct DataflowOrdering(
            pub Ident,
            pub CtrlLBrace,
            pub SepList1<
                (
                    SepList1<ExprId, CtrlComma>,
                    Ctrl, /* < */
                    SepList1<ExprId, CtrlComma>,
                ),
                CtrlSemi,
            >,
            pub CtrlRBrace,
        );

        #[derive(Debug)]
        pub struct LangDataflow(
            pub Kw,
            pub CtrlLBrace,
            pub Option<DataflowOrdering>,
            pub SepList1<DataflowItem, CtrlSemi>,
            pub CtrlRBrace,
        );
    }

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

    fn dataflow_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], DataflowItem, E> {
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

    pub fn lang_dataflow<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangDataflow, E> {
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
            .context("dataflow block")
            .parse(i)
    }
}

mod lang_initialize {
    use ast::*;

    use super::{
        *,
        lang_chp_hse::{ast::HseItemList, hse_body},
    };

    pub mod ast {
        use crate::utils::SepList1;

        use super::*;

        #[derive(Debug)]
        pub struct ActionItem(pub Ident, pub CtrlLBrace, pub HseItemList, pub CtrlRBrace);

        #[derive(Debug)]
        pub struct LangInitialize(
            pub Kw,
            pub CtrlLBrace,
            pub SepList1<ActionItem, CtrlSemi>,
            pub CtrlRBrace,
        );
    }

    // lang_initialize: "Initialize" "{" { action_items ";" }* "}"
    // action_items: ID "{" hse_body "}"
    pub fn lang_initialize<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangInitialize, E> {
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
    use ast::*;

    use super::*;

    pub mod ast {
        use crate::utils::SepList1;

        use super::*;

        #[derive(Debug)]
        pub struct DirectivePart(
            pub (Dir, Ctrl),
            pub Expr,
            pub Option<(CtrlComma, (Expr, Option<(CtrlComma, Expr)>))>,
        );

        #[derive(Debug)]
        pub enum SizingItem {
            Setup(Ident, CtrlLArrow, Expr),
            Directive(
                ExprId,
                CtrlLBrace,
                DirectivePart,
                Option<(CtrlSemi, DirectivePart)>,
                CtrlRBrace,
            ),
            DirectiveMacroLoop(MacroLoop<CtrlSemi, SepList1<SizingItem, CtrlSemi>>),
        }

        #[derive(Debug)]
        pub struct LangSizing(
            pub Kw,
            pub CtrlLBrace,
            pub SepList1<SizingItem, CtrlSemi>,
            pub CtrlRBrace,
        );
    }

    // size_setup: ID "<-" expr
    //           | NOTHING
    // size_directive: {excl}
    //                expr_id "{" dir expr [ "," ID ] [ "," expr ] [ ";" dir expr [ "," ID ] [ "," expr ] ] "}"
    //               | "(" ";" ID ":" !noreal expr [ ".." expr ] ":" { size_directive ";" }* ")"
    // size_body: { size_setup ";" }* { size_directive ";" }*
    // lang_size: "sizing" "{" [ size_body ] "}"

    fn directive_part<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], DirectivePart, E> {
        dir.then(expr)
            .then_opt(ctrl(',').then_cut(expr.then_opt(ctrl(',').then_cut(expr))))
            .map(|((a, b), c)| DirectivePart(a, b, c))
            .parse(i)
    }

    fn sizing_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], SizingItem, E> {
        let setup_item = ident
            .then(ctrl2('<', '-'))
            .then_cut(expr)
            .map(|((a, b), c)| SizingItem::Setup(a, b, c));
        let directive_item = expr_id
            .then(
                ctrl('{').then_cut(
                    directive_part
                        .then_opt(ctrl(';').then_cut(directive_part))
                        .then(ctrl('}')),
                ),
            )
            .map(|(a, (b, ((c, d), e)))| SizingItem::Directive(a, b, c, d, e));
        let directive_macro_loop = ctrl('(')
            .then_cut(tuple((
                ctrl(';'),
                ident,
                ctrl(':').then(expr_range).then(ctrl(':')),
                sizing_item.list1_sep_by(ctrl(';')).term_by(ctrl(')')),
            )))
            .map(|(a, (b, c, ((d, e), f), (g, h)))| MacroLoop(a, b, c, d, e, f, g, h))
            .map(SizingItem::DirectiveMacroLoop)
            .context("macro loop");
        alt((setup_item, directive_item, directive_macro_loop)).parse(i)
    }

    pub fn lang_sizing<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], LangSizing, E> {
        // TODO enforce size_sets are before sizing directives
        // This works because "size_setups" and "size_directives" are mutually exclusive.
        kw("sizing")
            .then_cut(ctrl('{').then(sizing_item.list1_sep_by(ctrl(';')).term_by(ctrl('}'))))
            .map(|(a, (b, (c, d)))| LangSizing(a, b, c, d))
            .context("sizing block")
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

