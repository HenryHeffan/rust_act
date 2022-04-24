use super::{
    basic::{ast::*, *},
    langs::{ast::*, *},
};
use crate::parser::utils::{MyParserExt, ParserExt2, ET};
use nom::{
    branch::alt,
    combinator::{eof, opt},
    sequence::tuple,
    IResult, Parser,
};
use nom_supreme::parser_ext::ParserExt;

pub mod ast {
    use super::*;
    use crate::parser::utils::SepList1;

    #[derive(Debug, Copy, Clone)]
    pub enum ChanDir<'a> {
        ReadOnly(Ctrl<'a>),
        WriteOnly(Ctrl<'a>),
        ReadWrite(Ctrl<'a>),
        WriteRead(Ctrl<'a>),
    }

    #[derive(Debug)]
    pub struct ChanType<'a>(
        pub Kw<'a>,
        pub Option<ChanDir<'a>>,
        pub CtrlLParen<'a>,
        pub PhysicalInstType<'a>,
        pub Option<(CtrlComma<'a>, PhysicalInstType<'a>)>,
        pub CtrlRParen<'a>,
        pub Option<ChanDir<'a>>,
    );

    #[derive(Debug)]
    pub enum NonChanTypeName<'a> {
        Int,
        Ints,
        Bool,
        Enum,
        QualifiedName(QualifiedName<'a>),
    }

    #[derive(Debug)]
    pub struct NonChanType<'a>(
        pub NonChanTypeName<'a>,
        pub Option<ChanDir<'a>>,
        pub  Option<(
            CtrlLAngBrace<'a>,
            SepList1<TemplateArg<'a>, CtrlComma<'a>>,
            CtrlRAngBrace<'a>,
        )>,
    );

    #[derive(Debug)]
    pub enum PhysicalInstType<'a> {
        ChanType(Box<ChanType<'a>>),
        NonChanType(NonChanType<'a>),
    }

    #[derive(Debug)]
    pub enum TemplateArg<'a> {
        // UserType(UserType<'a>),
        PhysType(CtrlAtSign<'a>, PhysicalInstType<'a>),
        ArrayedExprs(ArrayedExprs<'a>),
    }

    pub type IFaceInstType<'a> = PhysicalInstType<'a>; // UserType<'a>;

    #[derive(Debug)]
    pub enum InstType<'a> {
        Phys(PhysicalInstType<'a>),
        Param(ParamInstType<'a>),
    }

    #[derive(Debug)]
    pub enum ParamInstType<'a> {
        PInt(Kw<'a>),
        PBool(Kw<'a>),
        PReal(Kw<'a>),
        PType(Kw<'a>, CtrlLParen<'a>, PhysicalInstType<'a>, CtrlRParen<'a>), // PType(UserType<'a>),
    }

    #[derive(Debug)]
    pub enum FuncRetType<'a> {
        Phys(PhysicalInstType<'a>),
        Param(ParamInstType<'a>),
    }

    // Types for Connections, Instances, and Aliases

    #[derive(Debug)]
    pub enum PortConnSpec<'a> {
        Named(SepList1<(CtrlDot<'a>, Ident<'a>, CtrlEquals<'a>, ArrayedExprs<'a>), CtrlComma<'a>>),
        Unnamed(SepList1<Option<ArrayedExprs<'a>>, CtrlComma<'a>>),
    }

    #[derive(Debug)]
    pub struct Connection<'a>(
        pub Ident<'a>,
        pub Vec<(CtrlLBracket<'a>, Expr<'a>, CtrlRBracket<'a>)>,
        pub Option<(CtrlLParen<'a>, PortConnSpec<'a>, CtrlRParen<'a>)>,
        pub Option<(CtrlAtSign<'a>, BracketedAttrList<'a>)>,
        pub CtrlSemi<'a>,
    );
    #[derive(Debug)]
    pub struct InstanceId<'a>(
        pub Ident<'a>,
        pub Vec<(CtrlLBracket<'a>, ExprRange<'a>, CtrlRBracket<'a>)>,
        pub Option<(CtrlLParen<'a>, PortConnSpec<'a>, CtrlRParen<'a>)>,
        pub Option<(CtrlAtSign<'a>, BracketedAttrList<'a>)>,
        pub Vec<(CtrlEquals<'a>, ArrayedExprs<'a>)>,
    );
    #[derive(Debug)]
    pub struct Instance<'a>(
        pub InstType<'a>,
        pub SepList1<InstanceId<'a>, CtrlComma<'a>>,
        pub CtrlSemi<'a>,
    );
    #[derive(Debug)]
    pub struct Alias<'a>(
        pub ArrayedExprIds<'a>,
        pub CtrlEquals<'a>,
        pub ArrayedExprs<'a>,
        pub CtrlSemi<'a>,
    );

    #[derive(Debug)]
    pub enum AliasConnOrInst<'a> {
        Alias(Alias<'a>),
        Connection(Connection<'a>),
        Instance(Instance<'a>),
    }

    // Types for "base_items"

    #[derive(Debug)]
    pub enum GuardedClause<'a> {
        Expr(Expr<'a>, CtrlLArrow<'a>, Vec<BaseItem<'a>>),
        Else(Kw<'a>, CtrlLArrow<'a>, Vec<BaseItem<'a>>),
        MacroLoop(
            CtrlLParen<'a>,
            Ctrl<'a>, // []
            Ident<'a>,
            CtrlColon<'a>,
            ExprRange<'a>,
            CtrlColon<'a>,
            Expr<'a>,
            CtrlLArrow<'a>,
            Vec<BaseItem<'a>>,
            CtrlRParen<'a>,
        ),
    }

    #[derive(Debug)]
    pub struct Conditional<'a>(
        pub CtrlLBracket<'a>,
        pub SepList1<GuardedClause<'a>, Ctrl<'a> /*[]*/>,
        pub CtrlRBracket<'a>,
    );
    #[derive(Debug)]
    pub struct BaseDynamicLoop<'a>(
        pub Ctrl<'a>, /* `*[` */
        pub SepList1<GuardedClause<'a>, Ctrl<'a> /*[]*/>,
        pub CtrlRBracket<'a>,
    );

    #[derive(Debug, Clone, Copy)]
    pub enum ConnOp<'a> {
        Equal(Ctrl<'a>),
        NotEqual(Ctrl<'a>),
    }

    #[derive(Debug)]
    pub enum AssertionPart<'a> {
        Expr(Expr<'a>, Option<(CtrlColon<'a>, StrTok<'a>)>),
        Conn(ExprId<'a>, ConnOp<'a>, ExprId<'a>, Option<(CtrlColon<'a>, StrTok<'a>)>),
    }
    #[derive(Debug)]
    pub struct Assertion<'a>(
        pub CtrlLBrace<'a>,
        pub AssertionPart<'a>,
        pub CtrlRBrace<'a>,
        pub CtrlSemi<'a>,
    );

    #[derive(Debug)]
    pub struct DebugOutput<'a>(
        pub Ctrl<'a>, /* ${ */
        pub SepList1<ExprOrStr<'a>, CtrlComma<'a>>,
        pub CtrlRBrace<'a>,
        pub CtrlSemi<'a>,
    );

    #[derive(Debug)]
    pub struct BaseMacroLoop<'a>(
        pub CtrlLParen<'a>,
        pub Option<CtrlSemi<'a>>,
        pub Ident<'a>,
        pub CtrlColon<'a>,
        pub ExprRange<'a>,
        pub CtrlColon<'a>,
        pub Vec<BaseItem<'a>>,
        pub CtrlRParen<'a>,
    );

    #[derive(Debug)]
    pub enum BaseItem<'a> {
        Instance(Instance<'a>),
        Connection(Connection<'a>),
        Alias(Alias<'a>),
        DynamicLoop(BaseDynamicLoop<'a>),
        MacroLoop(BaseMacroLoop<'a>),
        Conditional(Conditional<'a>),
        Assertion(Assertion<'a>),
        DebugOutput(DebugOutput<'a>),
        // language bodies
        Chp(LangChp<'a>),
        Hse(LangHse<'a>),
        Prs(LangPrs<'a>),
        Spec(LangSpec<'a>),
        Refine(LangRefine<'a>),
        Sizing(LangSizing<'a>),
        Initialize(LangInitialize<'a>),
        Dataflow(LangDataflow<'a>),
    }

    #[derive(Debug)]
    pub struct LangRefine<'a>(
        pub Kw<'a>,
        pub CtrlLBracket<'a>,
        pub Vec<BaseItem<'a>>,
        pub CtrlRBracket<'a>,
    );

    // Types for "top level" items

    #[derive(Debug)]
    pub enum Import<'a> {
        String(StrTok<'a>),
        Namespace(QualifiedName<'a>, Option<(CtrlLArrow<'a>, Ident<'a>)>),
        Ident(Ident<'a>, Ctrl<'a> /* => */, Ident<'a>),
    }

    #[derive(Debug)]
    pub enum TopItem<'a> {
        Namespace(NamespaceDecl<'a>),
        Import(Kw<'a>, Import<'a>, CtrlSemi<'a>),
        Open(
            Kw<'a>,
            QualifiedName<'a>,
            Option<(CtrlLArrow<'a>, Ident<'a>)>,
            CtrlSemi<'a>,
        ),
        // These together compose the set of "definitions". Maybe they should be merged?
        DefTemplated(OptTemplateSpec<'a>, TempaltedDef<'a>),
        DefEnum(DefEnum<'a>),
        // Alias, Connection,Instance are the only ones shared with "BaseItem"
        Alias(Alias<'a>),
        Connection(Connection<'a>),
        Instance(Instance<'a>),
    }

    #[derive(Debug)]
    pub struct NamespaceDecl<'a>(
        pub Option<Kw<'a>>,
        pub Kw<'a>,
        pub Ident<'a>,
        pub CtrlLBrace<'a>,
        pub Vec<TopItem<'a>>,
        pub CtrlRBrace<'a>,
    );

    #[derive(Debug)]
    pub enum PortFormalListItem<'a> {
        Phys(PhysicalInstType<'a>, IdList<'a>),
        Param(ParamInstance<'a>),
    }

    #[derive(Debug)]
    pub struct ParenedPortFormalList<'a>(
        pub CtrlLParen<'a>,
        pub Option<SepList1<PortFormalListItem<'a>, CtrlSemi<'a>>>,
        pub CtrlRParen<'a>,
    );

    #[derive(Debug)]
    pub struct ParamInstance<'a>(pub ParamInstType<'a>, pub IdList<'a>);
    #[derive(Debug)]
    pub struct OptTemplateSpec<'a>(
        pub Option<Kw<'a>>,
        pub  Option<(
            Kw<'a>,
            CtrlLAngBrace<'a>,
            SepList1<ParamInstance<'a>, CtrlSemi<'a>>,
            CtrlRBrace<'a>,
        )>,
    );
    #[derive(Debug)]
    pub struct DefEnum<'a>(pub Kw<'a>, pub Ident<'a>, pub EnumBody<'a>);
    #[derive(Debug)]
    pub struct DefIFace<'a>(
        pub Kw<'a>,
        pub Ident<'a>,
        pub ParenedPortFormalList<'a>,
        pub CtrlSemi<'a>,
    );
    #[derive(Debug)]
    pub struct DefFunc<'a>(
        pub Kw<'a>,
        pub Ident<'a>,
        pub ParenedPortFormalList<'a>,
        pub CtrlColon<'a>,
        pub FuncRetType<'a>,
        pub FuncBody<'a>,
    );

    #[derive(Debug)]
    pub enum TempaltedDef<'a> {
        Proclike(DefProclike<'a>),
        Func(DefFunc<'a>),
        IFace(DefIFace<'a>),
    }

    #[derive(Debug)]
    pub struct OverrideOneSpec<'a>(pub PhysicalInstType<'a>, pub SepList1<Ident<'a>, CtrlComma<'a>>); // (UserType<'a>, Vec<Ident<'a>>);
    #[derive(Debug)]
    pub struct OverrideSpec<'a>(
        pub Ctrl<'a>, /* +{ */
        pub Vec<OverrideOneSpec<'a>>,
        pub CtrlRBrace<'a>,
    );
    #[derive(Debug)]
    pub struct InterfaceSpecItem<'a>(
        pub IFaceInstType<'a>,
        pub CtrlLBrace<'a>,
        pub SepList1<IdMap<'a>, CtrlComma<'a>>,
        pub CtrlRBrace<'a>,
    );
    #[derive(Debug)]
    pub struct InterfaceSpec<'a>(pub SepList1<InterfaceSpecItem<'a>, CtrlComma<'a>>);
    #[derive(Debug)]
    pub struct IdMap<'a>(pub Ident<'a>, pub CtrlLArrow<'a>, pub Ident<'a>);

    #[derive(Debug, Copy, Clone)]
    pub enum KwProclike<'a> {
        DefProc(Kw<'a>),
        DefCell(Kw<'a>),
        DefChan(Kw<'a>),
        DefData(Kw<'a>),
    }

    #[derive(Debug)]
    pub enum Method<'a> {
        Hse(Ident<'a>, CtrlLBrace<'a>, HseItemList<'a>, CtrlRBrace<'a>),
        Assign(Ident<'a>, CtrlEquals<'a>, Expr<'a>, CtrlSemi<'a>),
        Macro(
            Kw<'a>,
            Ident<'a>,
            ParenedPortFormalList<'a>,
            CtrlLBrace<'a>,
            Option<HseItemList<'a>>,
            CtrlRBrace<'a>,
        ),
    }

    #[derive(Debug)]
    pub struct DefProclike<'a>(
        pub KwProclike<'a>,
        pub Ident<'a>,
        pub Option<(Ctrl<'a> /* <: */, PhysicalInstType<'a>)>,
        pub ParenedPortFormalList<'a>,
        pub Option<(Ctrl<'a> /* :> */, InterfaceSpec<'a>)>,
        pub ProclikeBody<'a>,
    );

    #[derive(Debug)]
    pub struct MethodsBody<'a>(pub Kw<'a>, pub CtrlLBrace<'a>, pub Vec<Method<'a>>, pub CtrlRBrace<'a>);

    #[derive(Debug)]
    pub enum ProclikeBody<'a> {
        NoBody,
        WithBody(
            Option<OverrideSpec<'a>>,
            CtrlLBrace<'a>,
            Vec<BaseItem<'a>>,
            Option<MethodsBody<'a>>,
            CtrlRBrace<'a>,
        ),
    }

    #[derive(Debug)]
    pub struct FuncBody<'a>(pub ProclikeBody<'a>);

    // TODO add check in next pass to enforce right sort of things
    #[derive(Debug)]
    pub enum EnumBody<'a> {
        NoBody,
        WithBody(
            CtrlLBrace<'a>,
            SepList1<Ident<'a>, CtrlComma<'a>>,
            CtrlRBrace<'a>,
            CtrlSemi<'a>,
        ),
    }
    #[derive(Debug)]
    pub struct IdList<'a>(
        pub SepList1<(Ident<'a>, Vec<(CtrlLBracket<'a>, Expr<'a>, CtrlRBracket<'a>)>), CtrlComma<'a>>,
    );
}

use ast::*;

// chan_dir: {excl}
//          "?"
//         | "!"
//         | "?!"
//         | "!?"

fn chan_dir(i: &[u8]) -> IResult<&[u8], ChanDir, ET> {
    alt((
        ctrl2('!', '?').map(ChanDir::WriteRead),
        ctrl2('?', '!').map(ChanDir::ReadWrite),
        ctrl('!').map(ChanDir::WriteOnly),
        ctrl('?').map(ChanDir::ReadOnly),
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

pub fn physical_inst_type(i: &[u8]) -> IResult<&[u8], PhysicalInstType, ET> {
    let chan_type = kw("chan")
        .then_cut(
            chan_dir
                .opt()
                .then(ctrl('('))
                .then(physical_inst_type)
                .then_opt(ctrl(',').then_cut(physical_inst_type))
                .then(ctrl(')'))
                .then_opt(chan_dir),
        )
        .map(|(a, (((((b, c), d), e), f), g))| ChanType(a, b, c, d, e, f, g))
        .context("chan type");

    let arg = alt((
        ctrl('@')
            .then(physical_inst_type) // user_type
            .map(|(a, b)| TemplateArg::PhysType(a, b)),
        arrayed_exprs_no_gt.map(TemplateArg::ArrayedExprs),
    ));
    let template_args = arg.list1_sep_by(ctrl(',')).ang_braced().opt();
    let non_chan_type_name = alt((
        kw("int").map(|_| NonChanTypeName::Int),
        kw("ints").map(|_| NonChanTypeName::Ints),
        kw("bool").map(|_| NonChanTypeName::Bool),
        kw("enum").map(|_| NonChanTypeName::Enum),
        qualified_name.map(NonChanTypeName::QualifiedName),
    ));
    let non_chan_type = non_chan_type_name
        .then_opt(chan_dir)
        .then(template_args)
        .map(|((a, b), c)| NonChanType(a, b, c))
        .context("user type");

    alt((
        chan_type.map(Box::new).map(PhysicalInstType::ChanType),
        non_chan_type.map(PhysicalInstType::NonChanType),
    ))
    .context("physical inst type")
    .parse(i)
}

// inst_type: {excl}
//           physical_inst_type
//          | param_type
pub fn inst_type(i: &[u8]) -> IResult<&[u8], InstType, ET> {
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
//
// pub fn user_type(i: &[u8]) -> IResult<&[u8], UserType, ET> {
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

pub fn iface_inst_type(i: &[u8]) -> IResult<&[u8], IFaceInstType, ET> {
    physical_inst_type(i) // user_type(i)
}

// param_type: {excl}
//            "pint"
//           | "pbool"
//           | "preal"
//           | "ptype" "(" iface_inst_type ")"
pub fn param_inst_type(i: &[u8]) -> IResult<&[u8], ParamInstType, ET> {
    alt((
        kw("ptype")
            .then_cut(physical_inst_type.parened()) // user_type.parened()
            .map(|(a, (b, c, d))| ParamInstType::PType(a, b, c, d)),
        kw("pint").map(ParamInstType::PInt),
        kw("pbool").map(ParamInstType::PBool),
        kw("preal").map(ParamInstType::PReal),
    ))
    .context("param type")
    .parse(i)
}
// func_ret_type: {excl}
//               physical_inst_type
//              | param_type

pub fn func_ret_type(i: &[u8]) -> IResult<&[u8], FuncRetType, ET> {
    let phys = physical_inst_type.map(FuncRetType::Phys);
    let param = param_inst_type.map(FuncRetType::Param);
    param.or(phys).context("func return type").parse(i)
}

// port_conn_spec: { "." ID "=" arrayed_exprs "," }**
//               | { opt_arrayed_exprs "," }*
fn port_conn_spec(i: &[u8]) -> IResult<&[u8], PortConnSpec, ET> {
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
fn alias(i: &[u8]) -> IResult<&[u8], Alias, ET> {
    arrayed_expr_ids
        .then(ctrl('='))
        .then(arrayed_exprs)
        .then(ctrl(';'))
        .map(|(((a, b), c), d)| Alias(a, b, c, d))
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
fn connection(i: &[u8]) -> IResult<&[u8], Connection, ET> {
    let with_port_conn = ctrl('(')
        .then_cut(
            port_conn_spec
                .then(ctrl(')'))
                .then_opt(ctrl('@').then_cut(attr_list))
                .then(ctrl(';')),
        )
        .map(|(a, (((b, c), d), e))| (Some((a, b, c)), d, e));
    let no_port_conn = ctrl('@')
        .then_cut(attr_list.then(ctrl(';')))
        .map(|(a, (b, c))| (None, Some((a, b)), c));

    let brackets = expr.bracketed().many0().term_by_peek_not(ctrl('['));
    ident
        .then(brackets)
        .then(with_port_conn.or(no_port_conn))
        .map(|((a, b), (c, d, e))| Connection(a, b, c, d, e))
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
fn instance(i: &[u8]) -> IResult<&[u8], Instance, ET> {
    let extra_conns = ctrl('=').then(arrayed_exprs).many0().term_by_peek_not(ctrl('='));
    let bracketed_spare_ranges = expr_range.bracketed().many0().term_by_peek_not(ctrl('['));
    let instance_id = ident
        .then(bracketed_spare_ranges)
        .then_opt(
            ctrl('(')
                .then_cut(port_conn_spec.then(ctrl(')')))
                .map(|(a, (b, c))| (a, b, c)),
        )
        .then_opt(ctrl('@').then_cut(attr_list))
        .then(extra_conns)
        .map(|((((a, b), c), d), e)| InstanceId(a, b, c, d, e));
    inst_type
        .then_cut(instance_id.list1_sep_by(ctrl(',')).p().then(ctrl(';')))
        .map(|(a, (b, c))| Instance(a, b, c))
        .context("instance")
        .parse(i)
}

pub fn alias_conn_or_inst(i: &[u8]) -> IResult<&[u8], AliasConnOrInst, ET> {
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

fn guarded_clause<'a, T, OT>(peek_term_1: T, peek_term_2: T) -> impl Parser<&'a [u8], GuardedClause<'a>, ET<'a>>
where
    T: Parser<&'a [u8], OT, ET<'a>> + Clone + Copy,
{
    move |i| {
        let macro_branch = ctrl('(')
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
            .map(|((a, b), (c, d, e, f, g, h, (i, j)))| GuardedClause::MacroLoop(a, b, c, d, e, f, g, h, i, j))
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
pub fn base_item(i: &[u8]) -> IResult<&[u8], BaseItem, ET> {
    // NOTE: It is important that `[]` and `]` are both invalid starts to base items
    let conditional_gc = guarded_clause(ctrl2('[', ']'), ctrl(']'));
    let conditional = ctrl('[')
        .then_cut(conditional_gc.list1_sep_by(ctrl2('[', ']')).term_by(ctrl(']')))
        .map(|(a, (b, c))| Conditional(a, b, c));

    let base_macro_loop = ctrl('(')
        .then_cut(tuple((
            ctrl(';').opt(),
            ident,
            ctrl(':'),
            expr_range,
            ctrl(':'),
            base_item.many1().term_by(ctrl(')')),
        )))
        .map(|(a, (b, c, d, e, f, (g, h)))| BaseMacroLoop(a, b, c, d, e, f, g, h))
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
        ctrl3('=', '=', '=').map(ConnOp::Equal),
        ctrl3('!', '=', '=').map(ConnOp::NotEqual),
    ));
    let expr_assertion = expr
        .then_opt(ctrl(':').then(string))
        .map(|(a, b)| AssertionPart::Expr(a, b));
    let conn_assertion = expr_id
        .then(conn_op)
        .then(expr_id)
        .then_opt(ctrl(':').then(string))
        .map(|(((a, b), c), d)| AssertionPart::Conn(a, b, c, d));
    let assertion = ctrl('{')
        .then_cut(expr_assertion.or(conn_assertion))
        .then(ctrl('}'))
        .then(ctrl(';'))
        .map(|(((a, b), c), d)| Assertion(a, b, c, d))
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
pub fn lang_refine(i: &[u8]) -> IResult<&[u8], LangRefine, ET> {
    kw("refine")
        .then_cut(base_item.many1().braced())
        .map(|(a, (b, c, d))| LangRefine(a, b, c, d))
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

fn id_list(i: &[u8]) -> IResult<&[u8], IdList, ET> {
    let item = ident.then(expr.bracketed().many0().term_by_peek_not(ctrl('[')));

    item.list1_sep_by(ctrl(',')).p().map(IdList).parse(i)
}
fn parened_port_formal_list(i: &[u8]) -> IResult<&[u8], ParenedPortFormalList, ET> {
    let item = alt((
        physical_inst_type
            .then(id_list)
            .map(|(a, b)| PortFormalListItem::Phys(a, b)),
        param_instance.map(PortFormalListItem::Param),
    ));
    item.list1_sep_by(ctrl(';'))
        .opt()
        .parened()
        .map(|(a, b, c)| ParenedPortFormalList(a, b, c))
        .parse(i)
}

// param_inst: param_type id_list
fn param_instance(i: &[u8]) -> IResult<&[u8], ParamInstance, ET> {
    param_inst_type.then(id_list).map(|(a, b)| ParamInstance(a, b)).parse(i)
}

fn method(i: &[u8]) -> IResult<&[u8], Method, ET> {
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

fn proclike_body(i: &[u8]) -> IResult<&[u8], ProclikeBody, ET> {
    // user_type.then(ident.list1_sep_by(ctrl(',')).p());
    let one_override = physical_inst_type
        .then(ident.list1_sep_by(ctrl(',')).p())
        .map(|(a, b)| OverrideOneSpec(a, b));
    let overrides_block = one_override
        .many1()
        .delim_by(ctrl2('+', '{'), ctrl('}'))
        .map(|(a, b, c)| OverrideSpec(a, b, c));

    let methods_body = kw("methods")
        .then_cut(method.many1().braced())
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

    let no_body = ctrl(';').map(|_| ProclikeBody::NoBody);

    no_body.or(with_body).parse(i)
}

// TODO add a check in the next pass that (1) there is exactly 1 chp block per function and (2) that it comes last
fn func_body(i: &[u8]) -> IResult<&[u8], FuncBody, ET> {
    proclike_body.map(FuncBody).parse(i)
}

// This is closly coupled to the "top_items" parser below (in particular, where the "cut" is placed)
fn templateable_def(i: &[u8]) -> IResult<&[u8], TempaltedDef, ET> {
    let def_or_proc = alt((
        kw("defproc").map(KwProclike::DefProc),
        kw("defcell").map(KwProclike::DefCell),
        kw("defchan").map(KwProclike::DefChan),
        kw("defdata").map(KwProclike::DefData),
    ));

    let id_map = ident
        .then(ctrl2('-', '>'))
        .then(ident)
        .map(|((a, b), c)| IdMap(a, b, c));
    let interface_spec_item = iface_inst_type
        .then(id_map.list1_sep_by(ctrl(',')).braced())
        .map(|(a, (b, c, d))| InterfaceSpecItem(a, b, c, d));
    let interface_spec = interface_spec_item.list1_sep_by(ctrl(',')).p().map(InterfaceSpec);

    let def_proclike_after_spec = def_or_proc
        .then_cut(
            ident
                .then_opt(ctrl2('<', ':').then(physical_inst_type))
                .then(parened_port_formal_list)
                .then_opt(ctrl2(':', '>').then(interface_spec))
                .then(proclike_body),
        )
        .map(|(a, ((((b, c), d), e), f))| DefProclike(a, b, c, d, e, f));
    let def_func_after_spec = kw("function")
        .then_cut(
            ident
                .then(parened_port_formal_list)
                .then(ctrl(':'))
                .then(func_ret_type)
                .then(func_body),
        )
        .map(|(a, ((((b, c), d), e), f))| DefFunc(a, b, c, d, e, f));
    let def_iface_after_spec = kw("interface")
        .then_cut(ident.then(parened_port_formal_list).then(ctrl(';')))
        .map(|(a, ((b, c), d))| DefIFace(a, b, c, d));
    alt((
        def_proclike_after_spec.map(TempaltedDef::Proclike),
        def_func_after_spec.map(TempaltedDef::Func),
        def_iface_after_spec.map(TempaltedDef::IFace),
    ))
    .parse(i)
}

fn def_templated(i: &[u8]) -> IResult<&[u8], TopItem, ET> {
    // There are three cases for a "def_templated" block. We break them out here so that "cut"
    // provides nice error messages at this level of branching.

    // Option 1: `export [template_spec] templatedable_def`
    let def_templated1 = kw("export")
        .then_cut(
            opt(kw("template")
                .then(param_instance.list1_sep_by(ctrl(';')).ang_braced())
                .map(|(a, (b, c, d))| (a, b, c, d)))
            .then(templateable_def),
        )
        .map(|(a, (b, c))| TopItem::DefTemplated(OptTemplateSpec(Some(a), b), c))
        .context("define");

    // Option 2: `template_spec templatedable_def`
    let def_templated2 = kw("template")
        .then_cut(
            param_instance
                .list1_sep_by(ctrl(';'))
                .ang_braced()
                .then(templateable_def),
        )
        .map(|(a, ((b, c, d), e))| TopItem::DefTemplated(OptTemplateSpec(None, Some((a, b, c, d))), e))
        .context("define");

    // Option 3: `templatedable_def`
    let def_templated3 = templateable_def
        .map(|a| TopItem::DefTemplated(OptTemplateSpec(None, None), a))
        .context("define");

    alt((def_templated1, def_templated2, def_templated3))
        .context("def templated")
        .parse(i)
}

fn new_namespace(i: &[u8]) -> IResult<&[u8], TopItem, ET> {
    kw("export")
        .opt()
        .then(kw("namespace"))
        .then_cut(ident.then(top_item.many0().braced()))
        .map(|((a, b), (c, (d, e, f)))| TopItem::Namespace(NamespaceDecl(a, b, c, d, e, f)))
        .context("new namespace")
        .parse(i)
}

fn def_enum(i: &[u8]) -> IResult<&[u8], TopItem, ET> {
    let no_body = ctrl(';').map(|_| EnumBody::NoBody);
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

fn import(i: &[u8]) -> IResult<&[u8], TopItem, ET> {
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

    let import_namespace = qualified_name
        .then_opt(ctrl2('-', '>').then(ident))
        .map(|(ns, i)| Import::Namespace(ns, i));

    let import_ident = ident
        .then(ctrl2('=', '>'))
        .then(ident)
        .map(|((a, b), c)| Import::Ident(a, b, c));

    kw("import")
        .then_cut(import_string.or(import_namespace).or(import_ident).then(ctrl(';')))
        .map(|(a, (b, c))| TopItem::Import(a, b, c))
        .context("import")
        .parse(i)
}

fn open(i: &[u8]) -> IResult<&[u8], TopItem, ET> {
    // open_item: "open" qualified_ns "->" ID ";"
    //          | "open" qualified_ns ";"
    kw("open")
        .then_cut(qualified_name.then_opt(ctrl2('-', '>').then(ident)).then(ctrl(';')))
        .map(|(a, ((b, c), d))| TopItem::Open(a, b, c, d))
        .context("open")
        .parse(i)
}

pub fn top_item(i: &[u8]) -> IResult<&[u8], TopItem, ET> {
    let alias_conn_or_inst = alias_conn_or_inst.map(|v| match v {
        AliasConnOrInst::Alias(a) => TopItem::Alias(a),
        AliasConnOrInst::Connection(a) => TopItem::Connection(a),
        AliasConnOrInst::Instance(a) => TopItem::Instance(a),
    });

    alt((def_templated, def_enum, new_namespace, alias_conn_or_inst, import, open))
        .context("top level item")
        .parse(i)
}
pub fn top_level(i: &[u8]) -> nom::IResult<&[u8], Vec<TopItem>, ET> {
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
