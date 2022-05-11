use nom::{
    branch::alt,
    combinator::{eof, opt},
    IResult,
    Parser, sequence::tuple,
};
use nom_supreme::parser_ext::ParserExt;

use ast::*;

use crate::utils::{ET, MyParserExt, ParserExt2};

use super::{
    basic::{*, ast::*},
    langs::{*, ast::*},
};

pub mod ast {
    use crate::utils::SepList1;

    use super::*;

    #[derive(Debug, Copy, Clone)]
    pub enum ChanDir {
        ReadOnly(Ctrl),
        WriteOnly(Ctrl),
        ReadWrite(Ctrl),
        WriteRead(Ctrl),
    }

    #[derive(Debug)]
    pub struct ChanType(
        pub Kw,
        pub Option<ChanDir>,
        pub CtrlLParen,
        pub InstType,
        pub Option<(CtrlComma, InstType)>,
        pub CtrlRParen,
        pub Option<ChanDir>,
    );

    #[derive(Debug)]
    pub enum NonChanTypeName {
        Int(Kw),
        Ints(Kw),
        Bool(Kw),
        Enum(Kw),
        QualifiedName(QualifiedName),
    }

    #[derive(Debug)]
    pub enum TemplateArg {
        PhysType(CtrlAtSign, InstType),
        ArrayedExprs(ArrayedExprs),
    }

    #[derive(Debug)]
    pub struct NonChanType(
        pub NonChanTypeName,
        pub Option<ChanDir>,
        pub Option<(CtrlLAngBrace, SepList1<TemplateArg, CtrlComma>, CtrlRAngBrace)>,
    );

    #[derive(Debug)]
    pub enum InstType {
        ChanType(Box<ChanType>),
        NonChanType(NonChanType),
        Param(ParamInstType),
    }

    #[derive(Debug)]
    pub enum ParamInstType {
        PInt(Kw),
        PBool(Kw),
        PReal(Kw),
        PType(Kw, CtrlLParen, Box<InstType>, CtrlRParen),
    }

    // Types for Connections, Instances, and Aliases

    #[derive(Debug)]
    pub enum PortConnSpec {
        Named(SepList1<(CtrlDot, Ident, CtrlEquals, ArrayedExprs), CtrlComma>),
        Unnamed(SepList1<Option<ArrayedExprs>, CtrlComma>),
    }

    #[derive(Debug)]
    pub struct ConnectionId(
        pub Ident,
        pub Vec<(CtrlLBracket, ExprRange, CtrlRBracket)>,
        pub Option<(CtrlLParen, PortConnSpec, CtrlRParen)>,
        pub Option<(CtrlAtSign, BracketedAttrList)>,
    );

    #[derive(Debug)]
    pub struct Connection(pub ConnectionId, pub CtrlSemi);

    #[derive(Debug)]
    pub struct InstanceId(pub ConnectionId, pub Vec<(CtrlEquals, ArrayedExprs)>);

    #[derive(Debug)]
    pub struct Instance(pub InstType, pub SepList1<InstanceId, CtrlComma>, pub CtrlSemi);

    #[derive(Debug)]
    pub struct Alias(pub ArrayedExprIds, pub CtrlEquals, pub ArrayedExprs, pub CtrlSemi);

    #[derive(Debug)]
    pub enum AliasConnOrInst {
        Alias(Alias),
        Connection(Connection),
        Instance(Instance),
    }

    // Types for "base_items"

    #[derive(Debug)]
    pub enum GuardedClause {
        Expr(Expr, CtrlLArrow, Vec<TopItem>),
        Else(Kw, CtrlLArrow, Vec<TopItem>),
        MacroLoop(MacroLoop<Ctrl /* [] */, (Expr, CtrlLArrow, Vec<TopItem>)>),
    }

    #[derive(Debug)]
    pub struct Conditional(
        pub CtrlLBracket,
        pub SepList1<GuardedClause, Ctrl /*[]*/>,
        pub CtrlRBracket,
    );

    #[derive(Debug)]
    pub struct BaseDynamicLoop(
        pub Ctrl, /* `*[` */
        pub SepList1<GuardedClause, Ctrl /*[]*/>,
        pub CtrlRBracket,
    );

    #[derive(Debug, Clone, Copy)]
    pub enum ConnOp {
        Equal(Ctrl),
        NotEqual(Ctrl),
    }

    #[derive(Debug)]
    pub enum AssertionPart {
        Expr(Expr, Option<(CtrlColon, StrTok)>),
        Conn(ExprId, ConnOp, ExprId, Option<(CtrlColon, StrTok)>),
    }

    #[derive(Debug)]
    pub struct Assertion(pub CtrlLBrace, pub AssertionPart, pub CtrlRBrace, pub CtrlSemi);

    #[derive(Debug)]
    pub struct DebugOutput(
        pub Ctrl, /* ${ */
        pub SepList1<ExprOrStr, CtrlComma>,
        pub CtrlRBrace,
        pub CtrlSemi,
    );

    #[derive(Debug)]
    pub struct BaseMacroLoop(pub MacroLoop<Option<CtrlSemi>, Vec<TopItem>>);

    #[derive(Debug)]
    pub struct LangRefine(pub Kw, pub CtrlLBrace, pub Vec<TopItem>, pub CtrlRBrace);

    // Types for "top level" items

    #[derive(Debug)]
    pub enum Import {
        String(StrTok),
        Namespace(QualifiedName, Option<(CtrlLArrow, Ident)>),
        Ident(Ident, Ctrl /* => */, Ident),
    }

    #[derive(Debug)]
    pub enum TopItem {
        // These are used by both "TopItem" and "BaseItem"
        Instance(Instance),
        Connection(Connection),
        Alias(Alias),
        DynamicLoop(BaseDynamicLoop),
        MacroLoop(BaseMacroLoop),
        Conditional(Conditional),
        Assertion(Assertion),
        DebugOutput(DebugOutput),
        // These are used by top items
        Namespace(NamespaceDecl),
        Import(Kw, Import, CtrlSemi),
        Open(Kw, QualifiedName, Option<(CtrlLArrow, Ident)>, CtrlSemi),
        DefTemplated(OptTemplateSpec, ProclikeDecl, ProclikeBody),
        DefEnum(DefEnum),
        // These are used by base items
        Chp(LangChp),
        Hse(LangHse),
        Prs(LangPrs),
        Spec(LangSpec),
        Refine(LangRefine),
        Sizing(LangSizing),
        Initialize(LangInitialize),
        Dataflow(LangDataflow),
    }

    #[derive(Debug)]
    pub struct NamespaceDecl(
        pub Option<Kw>,
        pub Kw,
        pub Ident,
        pub CtrlLBrace,
        pub Vec<TopItem>,
        pub CtrlRBrace,
    );

    #[derive(Debug)]
    pub struct PortFormalListItem(pub InstType, pub IdList);

    #[derive(Debug)]
    pub struct ParenedPortFormalList(
        pub CtrlLParen,
        pub Option<SepList1<PortFormalListItem, CtrlSemi>>,
        pub CtrlRParen,
    );

    #[derive(Debug)]
    pub struct ParamInstance(pub ParamInstType, pub IdList);

    #[derive(Debug)]
    pub struct OptTemplateSpec(
        pub Option<Kw>,
        pub Option<(Kw, CtrlLAngBrace, SepList1<ParamInstance, CtrlSemi>, CtrlRBrace)>,
    );

    #[derive(Debug)]
    pub struct DefEnum(pub Kw, pub Ident, pub EnumBody);

    #[derive(Debug)]
    pub struct OverrideOneSpec(pub InstType, pub SepList1<Ident, CtrlComma>, pub CtrlSemi);

    #[derive(Debug)]
    pub struct OverrideSpec(pub Ctrl /* +{ */, pub Vec<OverrideOneSpec>, pub CtrlRBrace);

    #[derive(Debug)]
    pub struct InterfaceSpecItem(
        pub InstType,
        pub CtrlLBrace,
        pub SepList1<IdMap, CtrlComma>,
        pub CtrlRBrace,
    );

    #[derive(Debug)]
    pub struct InterfaceSpec(pub SepList1<InterfaceSpecItem, CtrlComma>);

    #[derive(Debug)]
    pub struct IdMap(pub Ident, pub CtrlLArrow, pub Ident);

    #[derive(Debug)]
    pub enum Method {
        Hse(Ident, CtrlLBrace, HseItemList, CtrlRBrace),
        Assign(Ident, CtrlEquals, Expr, CtrlSemi),
        Macro(
            Kw,
            Ident,
            ParenedPortFormalList,
            CtrlLBrace,
            Option<HseItemList>,
            CtrlRBrace,
        ),
    }

    #[derive(Debug, Copy, Clone)]
    pub enum KwProclikeKind {
        DefProc,
        DefCell,
        DefChan,
        DefData,
        DefFunc,
        DefIFace,
    }

    #[derive(Debug)]
    pub struct ProclikeDecl(
        pub (KwProclikeKind, Kw),
        pub Ident,
        pub Option<(Ctrl /* <: */, InstType)>,
        pub ParenedPortFormalList,
        pub Option<(Ctrl /* :> */, InterfaceSpec)>,
        pub Option<(CtrlColon, InstType)>,
    );

    #[derive(Debug)]
    pub struct MethodsBody(pub Kw, pub CtrlLBrace, pub Vec<Method>, pub CtrlRBrace);

    #[derive(Debug)]
    pub enum ProclikeBody {
        NoBody(CtrlSemi),
        WithBody(
            Option<OverrideSpec>,
            CtrlLBrace,
            Vec<TopItem>,
            Option<MethodsBody>,
            CtrlRBrace,
        ),
    }

    // TODO add check in next pass to enforce right sort of things
    #[derive(Debug)]
    pub enum EnumBody {
        NoBody(CtrlSemi),
        WithBody(CtrlLBrace, SepList1<Ident, CtrlComma>, CtrlRBrace, CtrlSemi),
    }

    #[derive(Debug)]
    pub struct IdList(pub SepList1<(Ident, Vec<(CtrlLBracket, Expr, CtrlRBracket)>), CtrlComma>);
}

// chan_dir: {excl}
//          "?"
//         | "!"
//         | "?!"
//         | "!?"

fn chan_dir<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ChanDir, E> {
    alt((
        ctrl2('!', '?').p().map(ChanDir::WriteRead),
        ctrl2('?', '!').p().map(ChanDir::ReadWrite),
        ctrl('!').p().map(ChanDir::WriteOnly),
        ctrl('?').p().map(ChanDir::ReadOnly),
    ))
        .context("chan dir")
        .parse(i)
}

// physical_inst_type: {excl}
//                    data_type
//                   | chan_type
//                   | user_type
// T_INT: {excl}
//       "int"
//      | "ints"
// data_type: {excl}
//           T_INT [ chan_dir ] [ "<" !endgt expr ">" !noendgt ]
//          | "bool" [ chan_dir ]
//          | "enum" [ chan_dir ] "<" expr ">"
// chan_type: "chan" [ chan_dir ] "(" physical_inst_type [ "," physical_inst_type ] ")" [ chan_dir ]
// inst_type: {excl}
//           physical_inst_type
//          | param_type
pub fn inst_type<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], InstType, E> {
    let chan_type = kw("chan")
        .then_cut(
            chan_dir
                .opt()
                .then(ctrl('('))
                .then(inst_type)
                .then_opt(ctrl(',').then_cut(inst_type))
                .then(ctrl(')'))
                .then_opt(chan_dir),
        )
        .map(|(a, (((((b, c), d), e), f), g))| ChanType(a, b, c, d, e, f, g))
        .context("chan type");

    let arg = alt((
        ctrl('@')
            .then(inst_type) // user_type
            .map(|(a, b)| TemplateArg::PhysType(a, b)),
        arrayed_exprs_no_gt.map(TemplateArg::ArrayedExprs),
    ));
    let template_args = arg.list1_sep_by(ctrl(',')).ang_braced().opt();
    let non_chan_type_name = alt((
        kw("int").p().map(NonChanTypeName::Int),
        kw("ints").p().map(NonChanTypeName::Ints),
        kw("bool").p().map(NonChanTypeName::Bool),
        kw("enum").p().map(NonChanTypeName::Enum),
        qualified_name.map(NonChanTypeName::QualifiedName),
    ));
    let non_chan_type = non_chan_type_name
        .then_opt(chan_dir)
        .then(template_args)
        .map(|((a, b), c)| NonChanType(a, b, c))
        .context("user type");

    alt((
        chan_type.map(Box::new).map(InstType::ChanType),
        non_chan_type.map(InstType::NonChanType),
        param_inst_type.map(InstType::Param),
    ))
        .context("inst type")
        .parse(i)
}

// user_type: qualified_type [ chan_dir ] [ template_args ]
// template_args: "<" !endgt { arrayed_exprs_or_type "," }* ">" !noendgt
// arrayed_exprs_or_type: {excl}
//                    "@" user_type
//                   | arrayed_exprs
// iface_inst_type: user_type
//
// pub fn user_type<'a, E:ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], UserType, E> {
//     let arg = ctrl('@')
//         .ignore_then(user_type)
//         .map(TemplateArg::UserType)
//         .or(arrayed_exprs_no_gt.map(TemplateArg::ArrayedExprs));
//     let template_args = arg.list1_sep_by(ctrl(',')).ang_braced().opt().map(|o| match o {
//         Some(o) => o,
//         None => Vec::new(),
//     });
//
//     qualified_name
//         .then(chan_dir.opt())
//         .then(template_args)
//         .map(|((a, b), c)| (a, b, c))
//         .context("user type")
//         .parse(i)
// }
// param_type: {excl}
//            "pint"
//           | "pbool"
//           | "preal"
//           | "ptype" "(" iface_inst_type ")"
pub fn param_inst_type<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ParamInstType, E> {
    alt((
        kw("ptype")
            .then_cut(inst_type.parened()) // user_type.parened()
            .map(|(a, (b, c, d))| ParamInstType::PType(a, b, Box::new(c), d)),
        kw("pint").p().map(ParamInstType::PInt),
        kw("pbool").p().map(ParamInstType::PBool),
        kw("preal").p().map(ParamInstType::PReal),
    ))
        .context("param type")
        .parse(i)
}

// func_ret_type: {excl}
//               physical_inst_type
//              | param_type
// port_conn_spec: { "." ID "=" arrayed_exprs "," }**
//               | { opt_arrayed_exprs "," }*
fn port_conn_spec<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], PortConnSpec, E> {
    let named = ctrl('.')
        .then(ident)
        .then(ctrl('='))
        .then(arrayed_exprs)
        .map(|(((a, b), c), d)| (a, b, c, d))
        .list1_sep_by(ctrl(','))
        .p()
        .map(PortConnSpec::Named);
    let unnamed = opt(arrayed_exprs)
        .list1_sep_by(ctrl(','))
        .p()
        .map(PortConnSpec::Unnamed);
    named.or(unnamed).context("port_conn_spec").parse(i)
}

// alias: arrayed_expr_ids "=" arrayed_exprs ";"
fn alias<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Alias, E> {
    arrayed_expr_ids
        .then(ctrl('='))
        .then(arrayed_exprs)
        .then(ctrl(';'))
        .map(|(((a, b), c), d)| Alias(a, b, c, d))
        .context("alias")
        .parse(i)
}

// TODO in a connection, enforce that the range is dense!
// dense_one_range: "[" expr "]"
// dense_range: {t-rec}
//             dense_one_range dense_range
//            | dense_one_range
// special_connection_id: ID [ dense_range ] "(" port_conn_spec ")" [ "@" attr_list ]
//                      | ID [ dense_range ] "@" attr_list
fn connection_id<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ConnectionId, E> {
    let opt_port_conn = ctrl('(')
        .then_cut(port_conn_spec.then(ctrl(')')))
        .map(|(a, (b, c))| (a, b, c))
        .opt();
    let opt_attr_list = ctrl('@').then_cut(cut_bracketed_attr_list).map(|(a, b)| (a, b)).opt();

    let bracketed_spare_ranges = expr_range.bracketed().many0().term_by_peek_not(ctrl('['));
    ident
        .then(bracketed_spare_ranges)
        .then(opt_port_conn)
        .then(opt_attr_list)
        .map(|(((a, b), c), d)| ConnectionId(a, b, c, d))
        .context("connection id")
        .parse(i)
}

// connection: special_connection_id ";"
fn connection<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Connection, E> {
    connection_id
        .then(ctrl(';'))
        .map(|(a, b)| Connection(a, b))
        .context("connection")
        .parse(i)
}

// sparse_range: {t-rec}
//              sparse_one_range sparse_range
//             | sparse_one_range
// sparse_one_range: "[" !noreal expr [ ".." expr ] "]"
// opt_extra_conn: [ "=" { arrayed_exprs "=" }** ]
// instance_id: ID [ sparse_range ] [ "(" port_conn_spec ")" ] [ "@" attr_list ] opt_extra_conn
// instance: inst_type { instance_id "," }* ";"
fn instance<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Instance, E> {
    let extra_conns = ctrl('=').then(arrayed_exprs).many0().term_by_peek_not(ctrl('='));
    let instance_id = connection_id.then(extra_conns).map(|(a, b)| InstanceId(a, b));
    inst_type
        .then_cut(instance_id.list1_sep_by(ctrl(',')).p().then(ctrl(';')))
        .map(|(a, (b, c))| Instance(a, b, c))
        .context("instance")
        .parse(i)
}

pub fn alias_conn_or_inst<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], AliasConnOrInst, E> {
    alt((
        alias.map(AliasConnOrInst::Alias),
        connection.map(AliasConnOrInst::Connection),
        instance.map(AliasConnOrInst::Instance),
    ))
        .parse(i)
}

// conditional: "[" guarded_cmds "]"
// guarded_cmds: { gc_1 "[]" }*
// TODO break the "(" "[]" ID ":" !noreal expr [ ".." expr ] pattern out into a seperate type
// gc_1: {excl} expr "->" base_item_list
//     | "(" "[]" ID ":" !noreal expr [ ".." expr ] ":" expr "->" base_item_list ")"
//     | "else" "->" base_item_list
// "*[" { gc_1 "[]" }* "]"
// assertion: "{" expr [ ":" STRING ] "}" ";"
//          | "{" expr_id conn_op expr_id [ ":" STRING ] "}" ";"
// conn_op: {excl}
//         "==="
//        | "!=="
// debug_output: "${" { chp_log_item "," }* "}" ";"
//             ;
// base_item_list: {t-rec}
//                base_item base_item_list
//               | base_item
// TODO can we merge this with base_body_item?
// base_item: instance
//          | connection
//          | alias
//          | language_body
//          | loop
//          | conditional
//          | assertion
//          | debug_output
// language_body: {excl}
//               lang_chp
//              | lang_hse
//              | lang_prs
//              | lang_spec
//              | lang_refine
//              | lang_size
//              | lang_initialize
//              | lang_dataflow
// base_macro_loop : "(" [ ";" ] ID ":" !noreal expr [ ".." expr ] ":" base_item_list ")"
// debug_output: "${" { chp_log_item "," }* "}" ";"
// top_level_body: {t-rec}
//      top_level_body_item top_level_body
//     | top_level_body_item
// top_level_body_item: namespace_management
//          | base_item
//          | definition
// ns_body: {t-rec}
//         ns_body_item ns_body
//        | ns_body_item
// ns_body_item: definition
//             | namespace_management
//             | namespace_other
// namespace_other: instance
//                | connection
//                | alias
// namespace_management: [ "export" ] "namespace" ID "{" [ ns_body ] "}"
// TODO enfoce that "connection" and "alias" nodes do not appear at the top level
// definition: defproc_or_cell
//           | defdata
//           | defchan
//           | deffunc
//           | defiface
// TODO          | defenum?
// deffunc:
//            [ template_spec ] "function" ID "(" function_formal_list ")" ":" func_ret_type func_body
// defiface: [ template_spec ] "interface" ID "(" [ port_formal_list ] ")" ";"
// defenum: "defenum" ID enum_body
// template_spec: [ "export" ] "template" "<" { param_inst ";" }* ">"
//              | "export"

fn guarded_clause<'a, T, OT, E: ET<'a>>(peek_term_1: T, peek_term_2: T) -> impl Parser<&'a [u8], GuardedClause, E>
    where
        T: Parser<&'a [u8], OT, E> + Clone + Copy,
{
    move |i| {
        let macro_branch = ctrl('(').p()
            .then(ctrl2('[', ']'))
            .then_cut(tuple((
                ident,
                ctrl(':'),
                expr_range,
                ctrl(':'),
                expr,
                ctrl2('-', '>'),
                base_item.many1().term_by(ctrl(')')),
            )))
            .map(|((a, b), (c, d, e, f, g, h, (i, j)))| {
                GuardedClause::MacroLoop(MacroLoop(a, b, c, d, e, f, (g, h, i), j))
            })
            .context("branch generator macro");

        let else_branch = kw("else")
            .then_cut(ctrl2('-', '>').then(base_item.many1().term_by_peek_alt2(peek_term_1, peek_term_2)))
            .map(|(a, (b, c))| GuardedClause::Else(a, b, c))
            .context("else branch");

        let expr_branch = expr
            .then(ctrl2('-', '>'))
            .then_cut(base_item.many1().term_by_peek_alt2(peek_term_1, peek_term_2))
            .map(|((a, b), c)| GuardedClause::Expr(a, b, c))
            .context("guarded branch");

        alt((macro_branch, else_branch, expr_branch))
            .context("guarded clause")
            .parse(i)
    }
}

/*
    // def_or_proc: {excl}
    //             "defproc"
    //            | "defcell"
    // defproc_or_cell: [ template_spec ] def_or_proc ID [ "<:" physical_inst_type ] "(" [ port_formal_list ] ")"
    //                  [ ":>" interface_spec ] proc_body
    // defchan: [ template_spec ] "defchan" ID "<:" physical_inst_type "(" [ port_formal_list ] ")" data_chan_body
    // data_chan_body: {excl}
    //                ";"
    //               | [ "+{" override_spec "}" ] "{" base_body [ methods_body ] "}"
    // proc_body:  ";"
    //          | [ "+{" override_spec "}" ] "{" def_body [ methods_body ] "}"
    // defdata: [ template_spec ] "deftype" ID [ "<:" physical_inst_type ] "(" [ port_formal_list ] ")" data_chan_body
    // TODO enforce a check in the next pass that the "Body" is the correct subset for data_chan_body
    // interface_spec: { interface_one_spec "," }*
    //               ;
    // interface_one_spec: iface_inst_type "{" { idmap "," }* "}"
    // idmap: ID "->" ID
    // methods_body: "methods" "{" [ method_list ] "}"
    // method_list: {t-rec}
    //             one_method method_list
    //            | one_method
    // override_spec: {t-rec}
    //               override_one_spec override_spec
    //              | override_one_spec
    // override_one_spec: user_type { ID "," }* ";"
    // def_body: base_item_list
    //         | NOTHING
    // one_method: ID "{" hse_body "}"
    //           | ID "=" expr ";"
    //           | "macro" ID "(" [ macro_formal_list ] ")" "{" [ chp_body ] "}"

    // port_formal_list: { single_port_item ";" }*
    // single_port_item: physical_inst_type id_list
    // macro_formal_list: { single_macro_port_item ";" }*
    // single_macro_port_item: physical_inst_type id_list
    // param_formal_list: { param_inst ";" }*
    // function_formal_list: {excl}
    //                      port_formal_list
    //                     | param_formal_list
    // func_body: {excl}
    //           ";"
    //          | "{" [ func_body_items ] "}"
    // func_body_items: alias_or_inst_list lang_chp
    // alias_or_inst_list: al_item alias_or_inst_list
    //                   | NOTHING
    // al_item: {excl}
    //         instance
    //        | assertion
    //        | debug_output
    // enum_body: {excl}
    //           ";"
    //          | "{"   { ID "," }*   "}" ";"
        // id_list: { ID [ dense_range ] "," }**
// dense_range: {t-rec}
//             dense_one_range dense_range
//            | dense_one_range
// dense_one_range: "[" expr "]"

    */

fn id_list<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], IdList, E> {
    let item = ident.then(expr.bracketed().many0().term_by_peek_not(ctrl('[')));

    item.list1_sep_by(ctrl(',')).p().map(IdList).parse(i)
}

fn parened_port_formal_list<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ParenedPortFormalList, E> {
    let item = inst_type.then(id_list).map(|(a, b)| PortFormalListItem(a, b));
    item.list1_sep_by(ctrl(';'))
        .opt()
        .parened()
        .map(|(a, b, c)| ParenedPortFormalList(a, b, c))
        .parse(i)
}

// param_inst: param_type id_list
fn param_instance<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ParamInstance, E> {
    param_inst_type.then(id_list).map(|(a, b)| ParamInstance(a, b)).parse(i)
}

fn method<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Method, E> {
    let method_hse = ident
        .then(ctrl('{'))
        .then_cut(hse_body.then(ctrl('}')))
        .map(|((a, b), (c, d))| Method::Hse(a, b, c, d));
    let assign = ident
        .then(ctrl('='))
        .then_cut(expr.then(ctrl(';')))
        .map(|((a, b), (c, d))| Method::Assign(a, b, c, d));
    let macro_ = kw("macro")
        .then_cut(ident.then(parened_port_formal_list).then(hse_body.opt().braced()))
        .map(|(a, ((b, c), (d, e, f)))| Method::Macro(a, b, c, d, e, f));
    method_hse.or(assign).or(macro_).parse(i)
}

// TODO for Func, add a check in the next pass that (1) there is exactly 1 chp block per function and (2) that it comes last
fn proclike_body<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ProclikeBody, E> {
    // user_type.then(ident.list1_sep_by(ctrl(',')).p());
    let one_override = inst_type
        .then(ident.list1_sep_by(ctrl(',')).term_by(ctrl(';')))
        .map(|(a, (b, c))| OverrideOneSpec(a, b, c));
    let overrides_block = ctrl2('+', '{')
        .then_cut(one_override.many0().term_by(ctrl('}')))
        .map(|(a, (b, c))| OverrideSpec(a, b, c));

    let methods_body = kw("methods")
        .then_cut(method.many0().braced())
        .map(|(a, (b, c, d))| MethodsBody(a, b, c, d));

    // after the base items, either there is a methods_body next (begining with kw `methods`), or there is a close brace

    let with_body = opt(overrides_block)
        .then(
            base_item
                .many0()
                .term_by_peek_alt2(kw("methods"), ctrl('}'))
                .then_opt(methods_body)
                .braced(),
        )
        .map(|(a, (b, (c, d), e))| ProclikeBody::WithBody(a, b, c, d, e));

    let no_body = ctrl(';').p().map(ProclikeBody::NoBody);

    no_body.or(with_body).parse(i)
}

// This is closly coupled to the "top_items" parser below (in particular, where the "cut" is placed)
fn proclike_decl<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ProclikeDecl, E> {
    let def_or_proc = alt((
        kw("defproc").p().map(|v| (KwProclikeKind::DefProc, v)),
        kw("defcell").p().map(|v| (KwProclikeKind::DefCell, v)),
        kw("defchan").p().map(|v| (KwProclikeKind::DefChan, v)),
        kw("deftype").p().map(|v| (KwProclikeKind::DefData, v)),
        kw("function").p().map(|v| (KwProclikeKind::DefFunc, v)),
        kw("interface").p().map(|v| (KwProclikeKind::DefIFace, v)),
    ));

    let id_map = ident
        .then(ctrl2('-', '>'))
        .then(ident)
        .map(|((a, b), c)| IdMap(a, b, c));
    let interface_spec_item = inst_type
        .then(id_map.list1_sep_by(ctrl(',')).braced())
        .map(|(a, (b, c, d))| InterfaceSpecItem(a, b, c, d));
    let interface_spec = interface_spec_item.list1_sep_by(ctrl(',')).p().map(InterfaceSpec);

    // we try to parse a return type after we try to parse an interface_spec because otherwise we would end up in the return_type case accedentally
    def_or_proc
        .then_cut(
            ident
                .then_opt(ctrl2('<', ':').then_cut(inst_type))
                .then(parened_port_formal_list)
                .then_opt(ctrl2(':', '>').then_cut(interface_spec))
                .then_opt(ctrl(':').then_cut(inst_type)),
        )
        .map(|(a, ((((b, c), d), e), f))| ProclikeDecl(a, b, c, d, e, f))
        .parse(i)
}

// lang_refine: "refine" "{" base_item_list "}"
pub fn lang_refine<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangRefine, E> {
    kw("refine")
        .then_cut(base_item.many1().braced())
        .map(|(a, (b, c, d))| LangRefine(a, b, c, d))
        .context("lang refine")
        .parse(i)
}

fn def_templated<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    let opt_templated_spec = kw("export").p()
        .opt()
        .then_opt(
            kw("template")
                .then_cut(param_instance.list1_sep_by(ctrl(';')).ang_braced())
                .map(|(a, (b, c, d))| (a, b, c, d)),
        )
        .map(|(a, b)| OptTemplateSpec(a, b));

    opt_templated_spec
        .then(proclike_decl)
        .then(proclike_body)
        .map(|((a, b), c)| TopItem::DefTemplated(a, b, c))
        .context("define")
        .parse(i)
}

fn new_namespace<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    kw("export").p()
        .opt()
        .then(kw("namespace"))
        .then_cut(ident.then(top_item.many0().braced()))
        .map(|((a, b), (c, (d, e, f)))| TopItem::Namespace(NamespaceDecl(a, b, c, d, e, f)))
        .context("new namespace")
        .parse(i)
}

fn def_enum<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    let no_body = ctrl(';').p().map(EnumBody::NoBody);
    let with_body = ident
        .list1_sep_by(ctrl(','))
        .braced()
        .then(ctrl(';'))
        .map(|((a, b, c), d)| EnumBody::WithBody(a, b, c, d));

    let enum_body = no_body.or(with_body);

    kw("defenum")
        .then_cut(ident.then(enum_body))
        .map(|(a, (b, c))| DefEnum(a, b, c))
        .map(TopItem::DefEnum)
        .context("def enum")
        .parse(i)
}

fn import<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    // imports_opens: {t-rec}
    //               import_open_item imports_opens
    //              | import_open_item
    // import_open_item: {excl}
    //                  import_item
    //                 | open_item
    // import_item: "import" STRING ";"
    //            | "import" [ "::" ] { ID "::" }* [ "->" ID ] ";"
    //            | "import" ID "=>" ID ";"
    let import_string = string.map(Import::String);

    let import_ident = ident
        .then(ctrl2('=', '>'))
        .then_cut(ident)
        .map(|((a, b), c)| Import::Ident(a, b, c));

    let import_namespace = qualified_name
        .then_opt(ctrl2('-', '>').then_cut(ident))
        .map(|(ns, i)| Import::Namespace(ns, i));

    kw("import")
        .then_cut(import_string.or(import_ident).or(import_namespace).then(ctrl(';')))
        .map(|(a, (b, c))| TopItem::Import(a, b, c))
        .context("import")
        .parse(i)
}

fn open<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    // open_item: "open" qualified_ns "->" ID ";"
    //          | "open" qualified_ns ";"
    kw("open")
        .then_cut(qualified_name.then_opt(ctrl2('-', '>').then(ident)).then(ctrl(';')))
        .map(|(a, ((b, c), d))| TopItem::Open(a, b, c, d))
        .context("open")
        .parse(i)
}

#[inline]
pub fn basic_item_helper<'a, E: ET<'a>>(is_top_parser: bool) -> impl Parser<&'a [u8], TopItem, E> {
    move |i: &'a [u8]| {
        // NOTE: It is important that `[]` and `]` are both invalid starts to base items
        let conditional_gc = guarded_clause(ctrl2('[', ']'), ctrl(']'));
        let conditional = ctrl('[')
            .then_cut(conditional_gc.list1_sep_by(ctrl2('[', ']')).term_by(ctrl(']')))
            .map(|(a, (b, c))| Conditional(a, b, c));

        let base_macro_loop = ctrl('(')
            .then_cut(tuple((
                ctrl(';').p().opt(),
                ident,
                ctrl(':'),
                expr_range,
                ctrl(':'),
                base_item.many1().term_by(ctrl(')')),
            )))
            .map(|(a, (b, c, d, e, f, (g, h)))| MacroLoop(a, b, c, d, e, f, g, h))
            .map(BaseMacroLoop)
            .context("macro loop");

        let dynamic_loop_gc = guarded_clause(ctrl2('[', ']'), ctrl(']'));
        let dynamic_loop_body = dynamic_loop_gc.list1_sep_by(ctrl2('[', ']'));
        let base_dynamic_loop = ctrl2('*', '[')
            .then_cut(dynamic_loop_body.term_by(ctrl(']')))
            .map(|(a, (b, c))| BaseDynamicLoop(a, b, c))
            .context("dynamic loop");

        let debug_output = ctrl2('$', '{')
            .then_cut(expr_or_str.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .then(ctrl(';'))
            .map(|((a, (b, c)), d)| DebugOutput(a, b, c, d));

        let conn_op = alt((
            ctrl3('=', '=', '=').p().map(ConnOp::Equal),
            ctrl3('!', '=', '=').p().map(ConnOp::NotEqual),
        ));
        let expr_assertion = expr
            .then_opt(ctrl(':').then(string))
            .map(|(a, b)| AssertionPart::Expr(a, b));
        let conn_assertion = expr_id
            .then(conn_op)
            .then_cut(expr_id)
            .then_opt(ctrl(':').then(string))
            .map(|(((a, b), c), d)| AssertionPart::Conn(a, b, c, d));
        let assertion = ctrl('{')
            .then_cut(conn_assertion.or(expr_assertion.cut()).then(ctrl('}')).then(ctrl(';')))
            .map(|(a, ((b, c), d))| Assertion(a, b, c, d))
            .context("assertion");

        let alias_conn_or_inst = alias_conn_or_inst.map(|v| match v {
            AliasConnOrInst::Alias(a) => TopItem::Alias(a),
            AliasConnOrInst::Connection(a) => TopItem::Connection(a),
            AliasConnOrInst::Instance(a) => TopItem::Instance(a),
        });

        let top_only_items = alt((
            // The all sort of start with an unambiguous keyword
            def_templated,
            def_enum,
            new_namespace,
            import,
            open,
        ));

        let mut either_items = alt((
            // All of these start with a keyword, and so are unambiguous
            lang_chp.map(TopItem::Chp),
            lang_hse.map(TopItem::Hse),
            lang_prs.map(TopItem::Prs),
            lang_spec.map(TopItem::Spec),
            lang_refine.map(TopItem::Refine),
            lang_sizing.map(TopItem::Sizing),
            lang_initialize.map(TopItem::Initialize),
            lang_dataflow.map(TopItem::Dataflow),
            // These each start with a unique starting symbol, so we can cut based on that token
            conditional.map(TopItem::Conditional),       // '['
            debug_output.map(TopItem::DebugOutput),      // '${'
            base_macro_loop.map(TopItem::MacroLoop),     // '('
            base_dynamic_loop.map(TopItem::DynamicLoop), // '*['
            uncut(assertion.map(TopItem::Assertion)),    // '{'  except an expression_id can start with this too :(
            // The other three (Alias, Connection, and Instance) are harder to distinguish
            alias_conn_or_inst,
        ));

        if is_top_parser {
            top_only_items.or(|i| (&mut either_items).parse(i)).parse(i)
        } else {
            either_items.parse(i)
        }
    }
}

pub fn base_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    basic_item_helper(false).context("top level item").parse(i)
}

pub fn top_item<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem, E> {
    basic_item_helper(true).context("top level item").parse(i)
}

#[flame]
pub fn top_level<'a, E: ET<'a>>(i: &'a [u8]) -> nom::IResult<&'a [u8], Vec<TopItem>, E> {
    top_item
        .many0()
        .terminated_fn(|| eof)
        .map(|(a, _)| a)
        .context("top level")
        .parse(i)
}

// short for alias_conn_or_inst
// NOTE: The parsing of these types is closely coupled with "TopItem" and "BaseItem" because of where we
// apply "cut" matters w.r.t the set of items "Connection", "Alias", and "Instance." (and their parsing order)
