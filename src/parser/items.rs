use super::{
    basic::{ast::*, *},
    langs::{ast::*, *},
};
use crate::parser::utils::{MyParserExt, ParserExt2, EE};
use nom::{
    branch::alt,
    combinator::{cut, eof, opt, peek},
    sequence::{preceded, tuple},
    IResult, Parser,
};
use nom_supreme::parser_ext::ParserExt;

pub mod ast {
    use super::*;

    #[derive(Debug, Copy, Clone)]
    pub enum ChanDir {
        ReadOnly,
        WriteOnly,
        ReadWrite,
        WriteRead,
    }

    pub type ChanType<'a> = (
        Option<ChanDir>,
        (PhysicalInstType<'a>, Option<PhysicalInstType<'a>>),
        Option<ChanDir>,
    );

    pub type UserType<'a> = (QualifiedName<'a>, Option<ChanDir>, Vec<TemplateArg<'a>>);

    #[derive(Debug)]
    pub enum PhysicalInstType<'a> {
        DataType(DataType<'a>),
        ChanType(Box<ChanType<'a>>),
        UserType(UserType<'a>),
    }

    #[derive(Debug)]
    pub enum TemplateArg<'a> {
        UserType(UserType<'a>),
        ArrayedExprs(ArrayedExprs<'a>),
    }

    pub type IFaceInstType<'a> = UserType<'a>;

    #[derive(Debug, Copy, Clone)]
    pub enum KwIntKind {
        Int,
        Ints,
    }

    #[derive(Debug)]
    pub enum DataType<'a> {
        Int(Option<ChanDir>, Option<Expr<'a>>),
        Bool(Option<ChanDir>),
        Enum(Option<ChanDir>, Expr<'a>),
    }

    #[derive(Debug)]
    pub enum InstType<'a> {
        Phys(PhysicalInstType<'a>),
        Param(ParamInstType<'a>),
    }

    #[derive(Debug)]
    pub enum ParamInstType<'a> {
        PInt,
        PBool,
        PReal,
        PType(UserType<'a>),
    }

    #[derive(Debug)]
    pub enum FuncRetType<'a> {
        Phys(PhysicalInstType<'a>),
        Param(ParamInstType<'a>),
    }

    // Types for Connections, Instances, and Aliases

    #[derive(Debug)]
    pub enum PortConnSpec<'a> {
        Named(Vec<(Ident<'a>, ArrayedExprs<'a>)>),
        Unnamed(Vec<Option<ArrayedExprs<'a>>>),
    }

    pub type Connection<'a> = (
        Ident<'a>,
        Vec<Expr<'a>>,
        (Option<PortConnSpec<'a>>, Option<AttrList<'a>>),
    );
    pub type InstanceId<'a> = (
        Ident<'a>,
        Option<Vec<ExprRange<'a>>>,
        Option<PortConnSpec<'a>>,
        Option<AttrList<'a>>,
        Option<Vec<ArrayedExprs<'a>>>,
    );
    pub type Instance<'a> = (InstType<'a>, Vec<InstanceId<'a>>);
    pub type Alias<'a> = (ArrayedExprIds<'a>, ArrayedExprs<'a>);

    #[derive(Debug)]
    pub enum AliasConnOrInst<'a> {
        Alias(Alias<'a>),
        Connection(Connection<'a>),
        Instance(Instance<'a>),
    }

    // Types for "base_items"

    #[derive(Debug)]
    pub enum GuardedClause<'a> {
        Expr(Expr<'a>, BaseItemList<'a>),
        Else(BaseItemList<'a>),
        MacroLoop((Ident<'a>, ExprRange<'a>, Expr<'a>, BaseItemList<'a>)),
    }

    pub type Conditional<'a> = Vec<GuardedClause<'a>>;
    pub type BaseDynamicLoop<'a> = Vec<GuardedClause<'a>>;

    #[derive(Debug, Clone, Copy)]
    pub enum ConnOp {
        Equal,
        NotEqual,
    }

    #[derive(Debug)]
    pub enum Assertion<'a> {
        Expr(Expr<'a>, Option<StrTok<'a>>),
        Conn(ExprId<'a>, ConnOp, ExprId<'a>, Option<StrTok<'a>>),
    }

    pub type DebugOutput<'a> = Vec<ExprOrStr<'a>>;

    #[derive(Debug)]
    pub enum BaseItem<'a> {
        Instance(Instance<'a>),
        Connection(Connection<'a>),
        Alias(Alias<'a>),
        DynamicLoop(BaseDynamicLoop<'a>),
        MacroLoop(MacroLoop<'a, bool, (), BaseItemList<'a>>),
        Conditional(Conditional<'a>),
        Assertion(Assertion<'a>),
        DebugOutput(DebugOutput<'a>),
        // language bodies
        Chp(LangChp<'a>),
        Hse(LangHse<'a>),
        Prs(LangPrs<'a>),
        Spec(LangSpec<'a>),
        Refine(BaseItemList<'a>),
        Sizing(LangSizing<'a>),
        Initialize(LangInitialize<'a>),
        Dataflow(LangDataflow<'a>),
    }

    pub type BaseItemList<'a> = Vec<BaseItem<'a>>;

    pub type LangRefine<'a> = BaseItemList<'a>;

    // Types for "top level" items

    #[derive(Debug)]
    pub enum TopItem<'a> {
        Namespace(NamespaceDecl<'a>),
        ImportString(StrTok<'a>),
        ImportNamespace(QualifiedName<'a>, Option<Ident<'a>>),
        ImportIdent(Ident<'a>, Ident<'a>),
        Open(QualifiedName<'a>, Option<Ident<'a>>),
        // These together compose the set of "definitions". Maybe they should be merged?
        DefTemplated(OptTemplateSpec<'a>, TempaltedDef<'a>),
        DefEnum(DefEnum<'a>),
        // Alias, Connection,Instance are the only ones shared with "BaseItem"
        Alias(Alias<'a>),
        Connection(Connection<'a>),
        Instance(Instance<'a>),
    }

    #[derive(Debug)]
    pub struct NamespaceDecl<'a> {
        pub export: bool,
        pub ident: Ident<'a>,
        pub body: Vec<TopItem<'a>>,
    }

    pub type PortFormalList<'a> = Vec<(PhysicalInstType<'a>, IdList<'a>)>;

    #[derive(Debug)]
    pub enum FunctionFormalList<'a> {
        Port(PortFormalList<'a>),
        Param(Vec<ParamInstance<'a>>),
    }

    pub type ParamInstance<'a> = (ParamInstType<'a>, IdList<'a>);
    pub type OptTemplateSpec<'a> = (bool, Option<Vec<ParamInstance<'a>>>);
    pub type DefEnum<'a> = (Ident<'a>, EnumBody<'a>);
    pub type DefIFace<'a> = (Ident<'a>, Option<PortFormalList<'a>>);
    pub type DefFunc<'a> = (Ident<'a>, FunctionFormalList<'a>, FuncRetType<'a>, FuncBody<'a>);

    #[derive(Debug)]
    pub enum TempaltedDef<'a> {
        Proclike(DefProclike<'a>),
        Func(DefFunc<'a>),
        IFace(DefIFace<'a>),
    }

    pub type OverrideSpec<'a> = Vec<OverrideOneSpec<'a>>;
    pub type OverrideOneSpec<'a> = (UserType<'a>, Vec<Ident<'a>>);
    pub type InterfaceSpec<'a> = Vec<(IFaceInstType<'a>, Vec<IdMap<'a>>)>;
    pub type IdMap<'a> = (Ident<'a>, Ident<'a>);

    #[derive(Debug, Copy, Clone)]
    pub enum KwProclike {
        DefProc,
        DefCell,
        DefChan,
        DefData,
    }

    #[derive(Debug)]
    pub enum Method<'a> {
        Hse(Ident<'a>, HseItemList<'a>),
        Assign(Ident<'a>, Expr<'a>),
        Macro(Ident<'a>, Option<PortFormalList<'a>>, Option<HseItemList<'a>>),
    }

    pub type DefProclike<'a> = (
        KwProclike,
        Ident<'a>,
        Option<PhysicalInstType<'a>>,
        Option<PortFormalList<'a>>,
        Option<InterfaceSpec<'a>>,
        ProclikeBody<'a>,
    );

    #[derive(Debug)]
    pub enum ProclikeBody<'a> {
        NoBody,
        WithBody(Option<OverrideSpec<'a>>, Vec<BaseItem<'a>>, Option<Vec<Method<'a>>>),
    }

    pub type FuncBody<'a> = ProclikeBody<'a>;

    // TODO add check in next pass to enforce right sort of things
    #[derive(Debug)]
    pub enum EnumBody<'a> {
        NoBody,
        WithBody(Vec<Ident<'a>>),
    }
    pub type IdList<'a> = Vec<(Ident<'a>, Vec<Expr<'a>>)>;
}

use ast::*;

// chan_dir: {excl}
//          "?"
//         | "!"
//         | "?!"
//         | "!?"

fn chan_dir<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ChanDir, E> {
    ctrl2('!', '?')
        .p()
        .value(ChanDir::WriteRead)
        .or(ctrl2('?', '!').p().value(ChanDir::ReadWrite))
        .or(ctrl('!').p().value(ChanDir::WriteOnly))
        .or(ctrl('?').p().value(ChanDir::ReadOnly))
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

pub fn physical_inst_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], PhysicalInstType<'a>, E> {
    let int_kw_type = kw("int").value(KwIntKind::Int).or(kw("ints").value(KwIntKind::Ints));

    let data_type = int_kw_type
        .ignore_then(chan_dir.opt())
        .then(expr_no_gt.ang_braced().opt())
        .map(|(a, b)| DataType::Int(a, b))
        .or(kw("bool").ignore_then(chan_dir.opt()).map(DataType::Bool))
        .or(kw("enum")
            .ignore_then(chan_dir.opt())
            .then(expr_no_gt.ang_braced())
            .map(|(a, b)| DataType::Enum(a, b)))
        .context("data type");

    let chan_type = kw("chan")
        .ignore_then(chan_dir.opt())
        .then(
            physical_inst_type
                .then(ctrl(',').ignore_then(physical_inst_type).opt())
                .parened(),
        )
        .then(chan_dir.opt())
        .map(|((a, b), c)| (a, b, c))
        .context("chan type");

    data_type
        .map(PhysicalInstType::DataType)
        .or(chan_type.map(|a| PhysicalInstType::ChanType(Box::new(a))))
        .or(user_type.map(PhysicalInstType::UserType))
        .context("physical inst type")
        .parse(i)
}

// inst_type: {excl}
//           physical_inst_type
//          | param_type
pub fn inst_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], InstType<'a>, E> {
    physical_inst_type
        .map(InstType::Phys)
        .or(param_inst_type.map(InstType::Param))
        .context("inst type")
        .parse(i)
}
// user_type: qualified_type [ chan_dir ] [ template_args ]
// template_args: "<" !endgt { arrayed_exprs_or_type "," }* ">" !noendgt
// arrayed_exprs_or_type: {excl}
//                    "@" user_type
//                   | arrayed_exprs
// iface_inst_type: user_type

pub fn user_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], UserType<'a>, E> {
    let arg = ctrl('@')
        .ignore_then(user_type)
        .map(TemplateArg::UserType)
        .or(arrayed_exprs_no_gt.map(TemplateArg::ArrayedExprs));
    let template_args = arg.list1_sep_by(ctrl(',')).ang_braced().opt().map(|o| match o {
        Some(o) => o,
        None => Vec::new(),
    });

    qualified_name
        .then(chan_dir.opt())
        .then(template_args)
        .map(|((a, b), c)| (a, b, c))
        .context("user type")
        .parse(i)
}

pub fn iface_inst_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], IFaceInstType<'a>, E> {
    user_type(i)
}

// param_type: {excl}
//            "pint"
//           | "pbool"
//           | "preal"
//           | "ptype" "(" iface_inst_type ")"
pub fn param_inst_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ParamInstType<'a>, E> {
    alt((
        kw("ptype")
            .ignore_then_cut(user_type.parened())
            .map(ParamInstType::PType),
        kw("pint").map(|_| ParamInstType::PInt),
        kw("pbool").map(|_| ParamInstType::PBool),
        kw("preal").map(|_| ParamInstType::PReal),
    ))
    .context("param type")
    .parse(i)
}
// func_ret_type: {excl}
//               physical_inst_type
//              | param_type

pub fn func_ret_type<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], FuncRetType<'a>, E> {
    let phys = physical_inst_type.map(FuncRetType::Phys);
    let param = param_inst_type.map(FuncRetType::Param);
    param.or(phys).context("func return type").parse(i)
}

// port_conn_spec: { "." ID "=" arrayed_exprs "," }**
//               | { opt_arrayed_exprs "," }*
fn port_conn_spec<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], PortConnSpec<'a>, E> {
    let named = ctrl('.')
        .precedes(ident)
        .and(preceded(ctrl('='), arrayed_exprs))
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
fn alias<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Alias<'a>, E> {
    arrayed_expr_ids
        .then_ignore(ctrl('='))
        .then_cut(arrayed_exprs.then_ignore(ctrl(';')))
        .context("alias")
        .parse(i)
}

// dense_one_range: "[" expr "]"
// dense_range: {t-rec}
//             dense_one_range dense_range
//            | dense_one_range
// special_connection_id: ID [ dense_range ] "(" port_conn_spec ")" [ "@" attr_list ]
//                      | ID [ dense_range ] "@" attr_list
// connection: special_connection_id ";"
fn connection<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Connection<'a>, E> {
    let with_port_conn = ctrl('(')
        .ignore_then_cut(
            port_conn_spec
                .then_ignore(ctrl(')'))
                .then(ctrl('@').ignore_then(attr_list).opt())
                .then_ignore(ctrl(';')),
        )
        .map(|(a, b)| (Some(a), b));
    let no_port_conn = ctrl('@')
        .ignore_then_cut(attr_list.then_ignore(ctrl(';')))
        .map(|b| (None, Some(b)));

    let brackets = expr.bracketed().many0().term_by_peek_not(ctrl('['));
    ident
        .then(brackets)
        .then(with_port_conn.or(no_port_conn))
        .map(|((a, b), c)| (a, b, c))
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
fn instance<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Instance<'a>, E> {
    let extra_conn = ctrl('=').ignore_then_cut(arrayed_exprs.list1_sep_by(ctrl('=')).p());
    let instance_id = ident
        .then(bracketed_spare_ranges.opt())
        .then(port_conn_spec.parened().opt())
        .then(ctrl('@').ignore_then(attr_list).opt())
        .then(extra_conn.opt())
        .map(|((((a, b), c), d), e)| (a, b, c, d, e));
    inst_type
        .then(cut(instance_id.list1_sep_by(ctrl(',')).p().then_ignore(ctrl(';'))))
        .context("instance")
        .parse(i)
}

pub fn alias_conn_or_inst<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], AliasConnOrInst<'a>, E> {
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

fn guarded_clause<'a, T, OT, E: EE<'a>>(peek_term_1: T, peek_term_2: T) -> impl Parser<&'a [u8], GuardedClause<'a>, E>
where
    T: Parser<&'a [u8], OT, E> + Clone + Copy,
{
    move |i| {
        let macro_branch = ctrl('(')
            .p()
            .then(ctrl2('[', ']'))
            .ignore_then_cut(tuple((
                ident,
                preceded(ctrl(':'), expr_range),
                preceded(ctrl(':'), expr.terminated(ctrl2('-', '>'))),
                cut(base_item.many1().terminated(ctrl(')'))),
            )))
            .context("branch generator macro");

        let else_branch = kw("else")
            .ignore_then(ctrl2('-', '>').ignore_then_cut(base_item.many1().term_by_peek_alt2(peek_term_1, peek_term_2)))
            .context("else branch");

        let expr_branch = expr
            .then_ignore(ctrl2('-', '>'))
            .then_cut(base_item.many1().term_by_peek_alt2(peek_term_1, peek_term_2))
            .context("guarded branch");

        alt((
            macro_branch.map(GuardedClause::MacroLoop),
            else_branch.map(GuardedClause::Else),
            expr_branch.map(|(a, b)| GuardedClause::Expr(a, b)),
        ))
        .context("guarded clause")
        .parse(i)
    }
}
pub fn base_item<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], BaseItem<'a>, E> {
    // NOTE: It is important that `[]` and `]` are both invalid starts to base items
    let conditional_gc = guarded_clause(ctrl2('[', ']'), ctrl(']'));
    let conditional = ctrl('[').ignore_then_cut(conditional_gc.list1_sep_by(ctrl2('[', ']')).term_by(ctrl(']')));

    let base_macro_loop = ctrl('(')
        .ignore_then_cut(tuple((
            ctrl(';').p().opt().map(|v| v.is_some()),
            ident,
            preceded(ctrl(':'), expr_range),
            none,
            preceded(ctrl(':'), base_item.many1().terminated(ctrl(')'))),
        )))
        .context("macro loop");

    let dynamic_loop_gc = guarded_clause(ctrl2('[', ']'), ctrl(']'));
    let dynamic_loop_body = dynamic_loop_gc.list1_sep_by(ctrl2('[', ']'));
    let base_dynamic_loop = ctrl2('*', '[')
        .ignore_then_cut(dynamic_loop_body.term_by(ctrl(']')))
        .context("dynamic loop");

    let debug_output_body = expr_or_str.list1_sep_by(ctrl(',')).term_by(ctrl('}'));
    let debug_output = ctrl2('$', '{')
        .ignore_then_cut(debug_output_body)
        .then_ignore(ctrl(';'));

    let conn_op = alt((
        ctrl3('=', '=', '=').p().value(ConnOp::Equal),
        ctrl3('!', '=', '=').p().value(ConnOp::NotEqual),
    ));
    let expr_assertion = expr
        .then(ctrl(':').ignore_then(string).opt())
        .map(|(a, b)| Assertion::Expr(a, b));
    let conn_assertion = expr_id
        .then(conn_op)
        .then(expr_id)
        .then(ctrl(':').ignore_then(string).opt())
        .map(|(((a, b), c), d)| Assertion::Conn(a, b, c, d));
    let assertion = ctrl('{')
        .ignore_then_cut(expr_assertion.or(conn_assertion))
        .then_ignore(ctrl('}'))
        .then_ignore(ctrl(';'))
        .context("assertion");

    let alias_conn_or_inst = alias_conn_or_inst.map(|v| match v {
        AliasConnOrInst::Alias(a) => BaseItem::Alias(a),
        AliasConnOrInst::Connection(a) => BaseItem::Connection(a),
        AliasConnOrInst::Instance(a) => BaseItem::Instance(a),
    });

    alt((
        // All of these start with a keyword, and so are unambiguous
        lang_chp.map(BaseItem::Chp),
        lang_hse.map(BaseItem::Hse),
        lang_prs.map(BaseItem::Prs),
        lang_spec.map(BaseItem::Spec),
        lang_refine.map(BaseItem::Refine),
        lang_sizing.map(BaseItem::Sizing),
        lang_initialize.map(BaseItem::Initialize),
        lang_dataflow.map(BaseItem::Dataflow),
        // These each start with a unique starting symbol, so we can cut based on that token
        conditional.map(BaseItem::Conditional),       // '['
        assertion.map(BaseItem::Assertion),           // '{'
        debug_output.map(BaseItem::DebugOutput),      // '${'
        base_macro_loop.map(BaseItem::MacroLoop),     // '('
        base_dynamic_loop.map(BaseItem::DynamicLoop), // '*['
        // The other three (Alias, Connection, and Instance) are harder to distinguish
        alias_conn_or_inst,
    ))
    .context("base item")
    .parse(i)
}

// lang_refine: "refine" "{" base_item_list "}"
pub fn lang_refine<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], LangRefine<'a>, E> {
    kw("refine")
        .ignore_then_cut(base_item.many1().braced())
        .context("lang refine")
        .parse(i)
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

fn id_list<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], IdList<'a>, E> {
    ident
        .then(expr.bracketed().many0().term_by_peek_not(ctrl('[')))
        .list1_sep_by(ctrl(','))
        .parse(i)
}
fn port_formal_list<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], PortFormalList<'a>, E> {
    physical_inst_type.then(id_list).list1_sep_by(ctrl(';')).parse(i)
}

// param_inst: param_type id_list
fn param_instance<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ParamInstance<'a>, E> {
    param_inst_type.then(id_list).parse(i)
}

fn function_formal_list<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], FunctionFormalList<'a>, E> {
    let param_formal_list = param_instance
        .list1_sep_by(ctrl(';'))
        .p()
        .map(FunctionFormalList::Param);
    let port = port_formal_list.map(FunctionFormalList::Port);
    param_formal_list.or(port).parse(i)
}

fn method<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Method<'a>, E> {
    let method_hse = ident
        .then_ignore(ctrl('{'))
        .then_cut(hse_body.then_ignore(ctrl('}')))
        .map(|(a, b)| Method::Hse(a, b));
    let assign = ident
        .then_ignore(ctrl('='))
        .then_cut(expr.then_ignore(ctrl(';')))
        .map(|(a, b)| Method::Assign(a, b));
    let macro_ = kw("macro")
        .ignore_then_cut(
            ident
                .then(port_formal_list.opt().parened())
                .then(hse_body.opt().braced()),
        )
        .map(|((a, b), c)| Method::Macro(a, b, c));
    method_hse.or(assign).or(macro_).parse(i)
}

fn proclike_body<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ProclikeBody<'a>, E> {
    let one_override = user_type.then(ident.list1_sep_by(ctrl(',')).p());
    let opt_overrides = one_override.many1().delim_by(ctrl2('+', '{'), ctrl('}')).opt();

    let methods_body = kw("methods").ignore_then(method.many1().braced());

    // after the base items, either there is a methods_body next (begining with kw `methods`), or there is a close brace
    let end_base_items_checker = || peek(kw("methods").value(()).or(ctrl('}').p().value(())));

    let with_body = opt_overrides
        .then(
            base_item
                .many0()
                .terminated_fn(end_base_items_checker)
                .then(methods_body.opt())
                .braced(),
        )
        .map(|(a, (b, c))| ProclikeBody::WithBody(a, b, c));

    let no_body = ctrl(';').p().map(|_| ProclikeBody::NoBody);

    no_body.or(with_body).parse(i)
}

// TODO add a check in the next pass that (1) there is exactly 1 chp block per function and (2) that it comes last
fn func_body<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], FuncBody<'a>, E> {
    proclike_body(i)
}

// This is closly coupled to the "top_items" parser below (in particular, where the "cut" is placed)
fn templateable_def<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TempaltedDef<'a>, E> {
    let def_or_proc = kw("defproc")
        .value(KwProclike::DefProc)
        .or(kw("defcell").value(KwProclike::DefCell))
        .or(kw("defchan").value(KwProclike::DefChan))
        .or(kw("defdata").value(KwProclike::DefData));

    let id_map = ident.then_ignore(ctrl2('-', '>')).then(ident);
    let interface_spec = iface_inst_type
        .then(id_map.list1_sep_by(ctrl(',')).braced())
        .list1_sep_by(ctrl(','))
        .p();

    let def_proclike_after_spec = def_or_proc
        .then_cut(tuple((
            ident,
            ctrl2('<', ':').ignore_then(physical_inst_type).opt(),
            port_formal_list.opt().parened(),
            ctrl2(':', '>').ignore_then(interface_spec).opt(),
            proclike_body,
        )))
        .map(|(a, (b, c, d, e, f))| TempaltedDef::Proclike((a, b, c, d, e, f)));
    let def_func_after_spec = kw("function")
        .ignore_then_cut(
            ident
                .then(function_formal_list.parened())
                .then_ignore(ctrl(':'))
                .then(func_ret_type)
                .then(func_body),
        )
        .map(|(((a, b), c), d)| TempaltedDef::Func((a, b, c, d)));
    let def_iface_after_spec = kw("interface")
        .ignore_then(ident)
        .then(port_formal_list.opt().parened())
        .then_ignore(ctrl(';'))
        .map(TempaltedDef::IFace);
    def_proclike_after_spec
        .or(def_func_after_spec)
        .or(def_iface_after_spec)
        .parse(i)
}

fn def_templated<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem<'a>, E> {
    // There are three cases for a "def_templated" block. We break them out here so that "cut"
    // provides nice error messages at this level of branching.

    // Option 1: `export [template_spec] templatedable_def`
    let def_templated1 = kw("export")
        .ignore_then_cut(
            kw("template")
                .ignore_then(param_instance.list1_sep_by(ctrl(';')).ang_braced())
                .opt()
                .then(templateable_def),
        )
        .map(|(params, def)| TopItem::DefTemplated((true, params), def))
        .context("define");

    // Option 2: `template_spec templatedable_def`
    let def_templated2 = kw("template")
        .ignore_then_cut(
            param_instance
                .list1_sep_by(ctrl(';'))
                .ang_braced()
                .then(templateable_def),
        )
        .map(|(params, def)| TopItem::DefTemplated((false, Some(params)), def))
        .context("define");

    // Option 3: `templatedable_def`
    let def_templated3 = templateable_def
        .map(|def| TopItem::DefTemplated((false, None), def))
        .context("define");

    alt((def_templated1, def_templated2, def_templated3))
        .context("def templated")
        .parse(i)
}

fn new_namespace<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem<'a>, E> {
    kw("export")
        .opt()
        .map(|v| v.is_some())
        .then_ignore(kw("namespace"))
        .then(ident)
        .then(top_item.many0().braced())
        .map(|((export, ident), body)| TopItem::Namespace(NamespaceDecl { export, ident, body }))
        .context("new namespace")
        .parse(i)
}

fn def_enum<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem<'a>, E> {
    let no_body = ctrl(';').p().map(|_| EnumBody::NoBody);
    let with_body = ident
        .list1_sep_by(ctrl(','))
        .braced()
        .then_ignore(ctrl(';'))
        .map(EnumBody::WithBody);

    let enum_body = no_body.or(with_body);

    kw("defenum")
        .ignore_then(ident)
        .then(enum_body)
        .map(TopItem::DefEnum)
        .context("def enum")
        .parse(i)
}

fn import<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem<'a>, E> {
    // imports_opens: {t-rec}
    //               import_open_item imports_opens
    //              | import_open_item
    // import_open_item: {excl}
    //                  import_item
    //                 | open_item
    // import_item: "import" STRING ";"
    //            | "import" [ "::" ] { ID "::" }* [ "->" ID ] ";"
    //            | "import" ID "=>" ID ";"
    let import_string = string.map(TopItem::ImportString);

    let import_namespace = qualified_name
        .then(ctrl2('-', '>').ignore_then(ident).opt())
        .map(|(ns, i)| TopItem::ImportNamespace(ns, i));

    let import_ident = ident
        .then_ignore(ctrl2('=', '>'))
        .then(ident)
        .map(|(i1, i2)| TopItem::ImportIdent(i1, i2));

    kw("import")
        .ignore_then_cut(
            import_string
                .or(import_namespace)
                .or(import_ident)
                .then_ignore(ctrl(';')),
        )
        .context("import")
        .parse(i)
}

fn open<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem<'a>, E> {
    // open_item: "open" qualified_ns "->" ID ";"
    //          | "open" qualified_ns ";"
    kw("open")
        .ignore_then_cut(
            qualified_name
                .then(ctrl2('-', '>').ignore_then(ident).opt())
                .then_ignore(ctrl(';')),
        )
        .map(|(ns, i)| TopItem::Open(ns, i))
        .context("open")
        .parse(i)
}

pub fn top_item<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], TopItem<'a>, E> {
    let alias_conn_or_inst = alias_conn_or_inst.map(|v| match v {
        AliasConnOrInst::Alias(a) => TopItem::Alias(a),
        AliasConnOrInst::Connection(a) => TopItem::Connection(a),
        AliasConnOrInst::Instance(a) => TopItem::Instance(a),
    });

    alt((def_templated, def_enum, new_namespace, alias_conn_or_inst, import, open))
        .context("top level item")
        .parse(i)
}
pub fn top_level<'a, E: EE<'a>>(i: &'a [u8]) -> nom::IResult<&'a [u8], Vec<TopItem<'a>>, E> {
    top_item.many0().terminated_fn(|| eof).context("top level").parse(i)
}

// short for alias_conn_or_inst
// NOTE: The parsing of these types is closely coupled with "TopItem" and "BaseItem" because of where we
// apply "cut" matters w.r.t the set of items "Connection", "Alias", and "Instance." (and their parsing order)
