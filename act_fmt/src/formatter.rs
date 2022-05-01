extern crate lex_parse;

use itertools::Itertools;

use lex_parse::{ast::*, ParseTree};

use super::utils::*;

impl PrAble for Ctrl {
    fn pr(&self) -> Pra {
        match self {
            Ctrl::Ctrl(c1) => Pra::from_tok(*c1),
            Ctrl::Ctrl2(c1, c2) => concat((Pra::from_tok(*c1), Pra::from_tok(*c2))),
            Ctrl::Ctrl3(c1, c2, c3) => concat((Pra::from_tok(*c1), Pra::from_tok(*c2), Pra::from_tok(*c3))),
        }
    }
}

impl PrAble for Ident {
    fn pr(&self) -> Pra {
        Pra::from_tok(self.0)
    }
}

impl PrAble for Kw {
    fn pr(&self) -> Pra {
        Pra::from_tok(self.0)
    }
}

impl PrAble for Num {
    fn pr(&self) -> Pra {
        Pra::from_tok(self.0)
    }
}

impl PrAble for StrTok {
    fn pr(&self) -> Pra {
        Pra::from_tok(self.0)
    }
}

impl PrAble for QualifiedName {
    fn pr(&self) -> Pra {
        let QualifiedName(cc, items) = self;
        let items = concat_sep1(items, nil());
        match cc {
            Some(cc) => concat((cc.pr(), items)),
            None => items.pr(),
        }
    }
}

impl PrAble for ChanDir {
    fn pr(&self) -> Pra {
        match self {
            ChanDir::ReadOnly(v) | ChanDir::WriteOnly(v) | ChanDir::ReadWrite(v) | ChanDir::WriteRead(v) => v.pr(),
        }
    }
}

impl PrAble for ChanType {
    fn pr(&self) -> Pra {
        let ChanType(kw, chan_dir, lparen, inst_type, inst_type_2, rparen, chan_dir_2) = self;
        concat((
            kw,
            chan_dir.map_or(nil(), |v| v.pr()),
            lparen,
            inst_type,
            inst_type_2
                .as_ref()
                .map_or(nil(), |(comma, v)| concat((comma.pr(), v.pr()))),
            rparen,
            chan_dir_2.map_or(nil(), |v| v.pr()),
        ))
    }
}

impl PrAble for TemplateArg {
    fn pr(&self) -> Pra {
        match self {
            TemplateArg::PhysType(at_sign, tp) => concat((at_sign, tp)),
            TemplateArg::ArrayedExprs(es) => es.pr(),
        }
    }
}

impl PrAble for NonChanTypeName {
    fn pr(&self) -> Pra {
        use NonChanTypeName::*;
        match self {
            Int(v) | Ints(v) | Bool(v) | Enum(v) => v.pr(),
            QualifiedName(v) => v.pr(),
        }
    }
}

impl PrAble for NonChanType {
    fn pr(&self) -> Pra {
        let NonChanType(kw, chan_dir, opt_template) = self;
        let opt_template = opt_template.as_ref().map_or(nil(), |(langle, params, rangle)| {
            concat((langle, concat_sep1(params, space()), rangle))
        });
        concat((kw, chan_dir.map_or(nil(), |v| v.pr()), opt_template))
    }
}

impl PrAble for ParamInstType {
    fn pr(&self) -> Pra {
        use ParamInstType::*;
        match self {
            PInt(v) | PBool(v) | PReal(v) => v.pr(),
            PType(kw, lparen, tp, rparen) => concat((kw.pr(), lparen.pr(), tp.pr(), rparen.pr())),
        }
    }
}

impl PrAble for InstType {
    fn pr(&self) -> Pra {
        match self {
            InstType::ChanType(chan_type) => chan_type.pr(),
            InstType::NonChanType(non_chan_type) => non_chan_type.pr(),
            InstType::Param(v) => v.pr(),
        }
    }
}

impl PrAble for FuncName {
    fn pr(&self) -> Pra {
        match self {
            FuncName::QualifiedName(v) => v.pr(),
            FuncName::Int(v) | FuncName::Bool(v) => v.pr(),
        }
    }
}

impl<T: PrAble> PrAble for Arrayed<T> {
    fn pr(&self) -> Pra {
        match self {
            Arrayed::Base(e) => e.pr(),
            Arrayed::Braces(lbrace, es, rbrace) => concat((lbrace, concat_sep1(es, space()), rbrace)),
            Arrayed::Hashes(es) => concat_sep1(es, nil()),
        }
    }
}

impl PrAble for BaseId {
    fn pr(&self) -> Pra {
        let BaseId { ident, brackets } = self;
        concat((
            ident,
            concat_map_vec(brackets, nil(), |(lbrac, e, rbrac)| concat((lbrac, e, rbrac))),
        ))
    }
}

impl PrAble for ExprId {
    fn pr(&self) -> Pra {
        let ExprId(ids) = self;
        concat_sep1(ids, nil())
    }
}

impl PrAble for ExprRange {
    fn pr(&self) -> Pra {
        let (e1, opt_e2) = self;
        match opt_e2 {
            None => e1.pr(),
            Some((dotdot, e2)) => concat((e1, dotdot, e2)),
        }
    }
}

impl PrAble for FuncTemplateParams {
    fn pr(&self) -> Pra {
        let FuncTemplateParams(langle, items, rangle) = self;
        concat((langle, concat_sep1(items, space()), rangle))
    }
}

impl PrAble for Expr {
    fn pr(&self) -> Pra {
        match self {
            Expr::Num(v) => v.pr(),
            Expr::Ident(v) => v.pr(),
            Expr::Unary(_, op, expr) => group((op, expr.pr())),
            // TODO should cut this into "layers" each of which is its own group
            Expr::Binary(my_op_kind, _, _, _) => {
                // expand out a whole sequence of binary ops using the same operator and put them into one group
                // TODO one group per order of operator precedence?
                let mut es = Vec::new();
                let mut e_left = self;
                loop {
                    match e_left {
                        Expr::Binary(op_kind, op, e1, e2) => {
                            if *op_kind != *my_op_kind {
                                break;
                            }
                            es.push((op, e2));
                            e_left = e1.as_ref();
                        }
                        _ => break,
                    }
                }
                let mut r = Vec::new();
                r.push(e_left.pr());
                for (op, e2) in es.into_iter().rev() {
                    r.push(space());
                    r.push(op.pr());
                    r.push(line());
                    r.push(e2.pr());
                }
                // println!("ending: {:?}", my_op_kind);
                // println!("{}", r.len());
                Pra::Concat(r).group()
            }
            Expr::Query(_, _, _, _, _) => {
                // expand out a whole sequence of queries and put them into one group
                let mut r = Vec::new();
                let mut e_right = self;
                loop {
                    match e_right {
                        Expr::Query(sel, qmark, o1, colon, o2) => {
                            r.push(group((sel.pr(), space(), qmark)));
                            r.push(line());
                            r.push(group((o1.pr(), space(), colon)));
                            r.push(line());
                            e_right = o2.as_ref();
                        }
                        _ => break,
                    }
                }
                r.push(e_right.pr().group());
                Pra::Concat(r).group()
            }
            Expr::BitField(id, lbrace, e_hi, dotdot_e_low, rbrace) => match dotdot_e_low {
                Some((dotdot, e_low)) => group((id.pr(), lbrace, e_hi.pr(), dotdot, e_low.pr(), rbrace)),
                None => group((id.pr(), lbrace, e_hi.pr(), rbrace)),
            },
            Expr::ArrAccess(id, lbrace, e_hi, dotdot_e_low, rbrace) => match dotdot_e_low {
                Some((dotdot, e_low)) => group((id.pr(), lbrace, e_hi.pr(), dotdot, e_low.pr(), rbrace)),
                None => group((id.pr(), lbrace, e_hi.pr(), rbrace)),
            },
            Expr::Concat(lbrace, items, rbrace) => group((
                lbrace,
                concat((line_(), concat_sep1(items, line()))).nest(NestAmt(2)),
                line_(),
                rbrace,
            )),
            Expr::Dot(lhs, dot, id) => group((lhs.pr(), dot, id)),
            Expr::Call(lhs, template_params, lparen, items, rparen) => {
                let template_params = template_params.as_ref().map_or(nil(), |v| v.pr());
                group((
                    lhs,
                    template_params,
                    lparen,
                    concat_sep1(items, space()).nest(NestAmt(2)),
                    rparen,
                ))
            }
            Expr::Parened(lparen, e, rparen) => {
                group((lparen, concat((line_(), e.pr())).nest(NestAmt(2)), line_(), rparen))
            }
            Expr::MacroLoop(lparen, (_, op), id, colon1, range, colon2, e, rparen) => group((
                lparen,
                op,
                space(),
                id,
                colon1,
                space(),
                range.pr(),
                colon2,
                space(),
                e.pr(),
                rparen,
            )),
        }
    }
}

impl PrAble for IdList {
    fn pr(&self) -> Pra {
        let IdList(sep_list) = self;
        concat_map_sep1(sep_list, space(), |(id, brackets)| {
            let brackets = concat_map(brackets, |(lbrac, e, rbrac)| concat((lbrac, e, rbrac)));
            concat((id, brackets))
        })
    }
}

impl PrAble for ParamInstance {
    fn pr(&self) -> Pra {
        let ParamInstance(param_inst_type, id_list) = self;
        concat((param_inst_type, space(), id_list))
    }
}

impl PrAble for OptTemplateSpec {
    fn pr(&self) -> Pra {
        let OptTemplateSpec(kw_export, template) = self;
        let export = match kw_export {
            Some(kw_export) => concat((kw_export.pr(), space())),
            None => nil(),
        };
        let template = match template {
            Some((kw_template, lbrace, params, rbrace)) => {
                let params = concat_sep1(params, space());
                concat((kw_template, lbrace, params.group().nest(NestAmt(2)), rbrace, space()))
            }
            None => nil(),
        };
        group((export, template))
    }
}

impl PrAble for PortFormalListItem {
    fn pr(&self) -> Pra {
        let PortFormalListItem(tp, ids) = self;
        concat((tp, space(), ids))
    }
}

impl PrAble for ParenedPortFormalList {
    fn pr(&self) -> Pra {
        let ParenedPortFormalList(lparen, items, rparen) = self;
        match items {
            Some(items) => group((
                lparen,
                concat((line_(), concat_sep1(items, line()))).nest(NestAmt(2)),
                line_(),
                rparen,
            )),
            None => concat((lparen, rparen)),
        }
    }
}

impl PrAble for IdMap {
    fn pr(&self) -> Pra {
        let IdMap(id, arrow, id2) = self;
        concat((id, space(), arrow, space(), id2))
    }
}

impl PrAble for InterfaceSpecItem {
    fn pr(&self) -> Pra {
        let InterfaceSpecItem(tp, lbrace, items, rbrace) = self;
        let items = concat_sep1(items, space());
        concat((
            tp,
            lbrace,
            items.group().nest(/*TODO GIVE GENERIC NAME*/ NestAmt(2)),
            rbrace,
        ))
    }
}

impl PrAble for InterfaceSpec {
    fn pr(&self) -> Pra {
        let InterfaceSpec(items) = self;
        concat_sep1(items, space())
    }
}

impl PrAble for ProclikeDecl {
    fn pr(&self) -> Pra {
        let ProclikeDecl((_, kw), name, derived_type, ports, spec, ret_type) = self;
        let derived_type = derived_type
            .as_ref()
            .map_or(nil(), |(ctrl, tp)| concat((space(), ctrl, space(), tp, soft_line())));
        let spec = spec
            .as_ref()
            .map_or(nil(), |(ctrl, tp)| concat((soft_line(), ctrl, space(), tp)));
        let ret_type = ret_type
            .as_ref()
            .map_or(nil(), |(colon, tp)| concat((soft_line(), colon, space(), tp)));
        group((kw, space(), name, derived_type, ports, spec, ret_type))
    }
}

impl PrAble for EnumBody {
    fn pr(&self) -> Pra {
        match self {
            EnumBody::NoBody(semi) => semi.pr(),
            EnumBody::WithBody(lbrace, items, rbrace, semi) => {
                concat((lbrace, concat_sep1(items, space()), rbrace, semi))
            }
        }
    }
}

impl PrChunkAble for DefEnum {
    fn prc(&self) -> PraChunk {
        let DefEnum(kw, id, body) = self;
        let p = concat((kw, space(), id, body));
        p.chunk()
    }
}

impl PrAble for OverrideOneSpec {
    fn pr(&self) -> Pra {
        let OverrideOneSpec(tp, names, semi) = self;
        concat((tp, space(), concat_sep1(names, space()), semi))
    }
}

impl PrAble for OverrideSpec {
    fn pr(&self) -> Pra {
        let OverrideSpec(lbrace, items, rbrace) = self;
        match items.len() {
            0 => group((lbrace, rbrace)), // TODO just return nil?
            _ => group((
                hard_line(),
                lbrace,
                concat((hard_line(), concat_vec(items, hard_line()))).nest(NestAmt(2)),
                hard_line(),
                rbrace,
            )),
        }
    }
}

impl PrAble for BracketedAttrList {
    fn pr(&self) -> Pra {
        let BracketedAttrList(lbrac, items, rbrac) = self;
        concat((
            lbrac,
            concat_map_sep1(items, space(), |(id, eq, e)| concat((id, eq, e))),
            rbrac,
        ))
    }
}

impl PrAble for PortConnSpec {
    fn pr(&self) -> Pra {
        match self {
            PortConnSpec::Named(items) => concat_map_sep1(items, space(), |(dot, id, eq, exprs)| {
                concat((dot, id, space(), eq, space(), exprs))
            }),
            PortConnSpec::Unnamed(items) => {
                concat_map_sep1(items, space(), |v| v.as_ref().map_or(nil(), |exprs| exprs.pr()))
            }
        }
    }
}

impl PrAble for ConnectionId {
    fn pr(&self) -> Pra {
        let ConnectionId(id, brackets, port_conn, attr_list) = self;
        let brackets = concat_map_vec(brackets, nil(), |(lbrac, e, rbrac)| concat((lbrac, e, rbrac)));
        concat((
            id,
            brackets,
            port_conn.as_ref().map_or(nil(), |(lparen, conn, rparen)| {
                group((lparen, concat((line_(), conn)).nest(NestAmt(2)), line_(), rparen))
            }),
            attr_list
                .as_ref()
                .map_or(nil(), |(at_sign, attrs)| concat((at_sign, attrs))),
        ))
    }
}

impl PrAble for InstanceId {
    fn pr(&self) -> Pra {
        let InstanceId(conn_id, assigns) = self;
        let assigns = concat_map_vec(assigns, nil(), |(eq, arrayed_id)| {
            concat((space(), eq, space(), arrayed_id))
        });
        concat((conn_id, assigns))
    }
}

impl PrChunkAble for Instance {
    fn prc(&self) -> PraChunk {
        let Instance(tp, ids, semi) = self;
        let p = concat((tp, space(), concat_sep1(ids, soft_line()).nest(NestAmt(2)), semi));
        p.chunk()
    }
}

impl PrChunkAble for Connection {
    fn prc(&self) -> PraChunk {
        let Connection(conn_id, semi) = self;
        concat((conn_id, semi)).chunk()
    }
}

impl PrChunkAble for Alias {
    fn prc(&self) -> PraChunk {
        let Alias(ids, eq, exprs, semi) = self;
        let p = concat((ids, space(), eq, space(), exprs, semi));
        p.chunk()
    }
}

impl PrAble for GuardedClause {
    fn pr(&self) -> Pra {
        match self {
            GuardedClause::Expr(e, arrow, items) => {
                let items = concat_chunks(hard_line(), items.iter().map(|v| v.prc()).collect(), None, NestAmt(2));
                concat((e, space(), arrow, space(), items))
            }
            GuardedClause::Else(kw, arrow, items) => {
                let items = concat_chunks(hard_line(), items.iter().map(|v| v.prc()).collect(), None, NestAmt(2));
                concat((kw, space(), arrow, space(), items))
            }
            GuardedClause::MacroLoop(MacroLoop(lparen, ctrl, id, colon1, range, colon2, (e, arrow, items), rparen)) => {
                let items = concat_chunks(hard_line(), items.iter().map(|v| v.prc()).collect(), None, NestAmt(2));
                let prefix = concat((
                    lparen,
                    ctrl,
                    space(),
                    id,
                    space(),
                    colon1,
                    space(),
                    range,
                    space(),
                    colon2,
                ));
                group((prefix, space(), e, space(), arrow, items, space(), rparen))
            }
        }
    }
}

fn format_base_conditional_like(
    lbrac: Pra,
    extra_space: Pra,
    items: &SepList1<GuardedClause, Ctrl>,
    rbrac: Pra,
) -> Pra {
    if items.items.len() == 1 {
        assert_eq!(items.seps.len(), 0);
        concat((lbrac, space(), &items.items[0], space(), rbrac))
    } else {
        let seps = vec![extra_space].into_iter().chain(items.seps.iter().map(|v| v.pr()));
        let chunks = seps
            .zip_eq(items.items.iter())
            .map(|(sep, clause)| concat((sep, space(), clause)).chunk())
            .collect_vec();

        concat((lbrac, concat_chunks(line(), chunks, Some(rbrac), NestAmt(0))))
    }
}

impl PrChunkAble for BaseDynamicLoop {
    fn prc(&self) -> PraChunk {
        let BaseDynamicLoop(lbrac, items, rbrac) = self;
        format_base_conditional_like(lbrac.pr(), nil(), items, rbrac.pr()).chunk()
    }
}

impl PrChunkAble for Conditional {
    fn prc(&self) -> PraChunk {
        let Conditional(lbrac, items, rbrac) = self;
        format_base_conditional_like(lbrac.pr(), space(), items, rbrac.pr()).chunk()
    }
}

impl PrChunkAble for BaseMacroLoop {
    fn prc(&self) -> PraChunk {
        let BaseMacroLoop(MacroLoop(lparen, semi, id, colon1, range, colon2, items, rparen)) = self;
        let items = items.iter().map(|v| v.prc()).collect_vec();
        let semi = semi.map_or(nil(), |v| v.pr());
        let chunks_then_close_paren = concat_chunks(line(), items, Some(rparen.pr()), NestAmt(2));
        group((
            lparen,
            semi,
            space(),
            id,
            space(),
            colon1,
            space(),
            range,
            colon2,
            chunks_then_close_paren,
        ))
            .chunk()
    }
}

impl PrAble for ConnOp {
    fn pr(&self) -> Pra {
        match self {
            ConnOp::Equal(v) | ConnOp::NotEqual(v) => v.pr(),
        }
    }
}

impl PrAble for AssertionPart {
    fn pr(&self) -> Pra {
        match self {
            AssertionPart::Expr(e, label) => {
                let label = label
                    .as_ref()
                    .map_or(nil(), |(colon, label)| concat((space(), colon, space(), label)));
                concat((e, label))
            }
            AssertionPart::Conn(id1, conn_op, id2, label) => {
                let label = label
                    .as_ref()
                    .map_or(nil(), |(colon, label)| concat((space(), colon, space(), label)));
                concat((id1, space(), conn_op, space(), id2, label))
            }
        }
    }
}

impl PrChunkAble for Assertion {
    fn prc(&self) -> PraChunk {
        let Assertion(lbrace, assertion_part, rbrace, semi) = self;
        concat((lbrace, space(), assertion_part, space(), rbrace, semi)).chunk()
    }
}

impl PrAble for ExprOrStr {
    fn pr(&self) -> Pra {
        match self {
            ExprOrStr::Expr(v) => v.pr(),
            ExprOrStr::Str(v) => v.pr(),
        }
    }
}

impl PrChunkAble for DebugOutput {
    fn prc(&self) -> PraChunk {
        let DebugOutput(lbrace, exprs, rbrace, semi) = self;
        let exprs = concat_sep1(exprs, space());
        concat((lbrace, space(), exprs, space(), rbrace, semi)).chunk()
    }
}

impl PrAble for SupplySpec {
    fn pr(&self) -> Pra {
        let SupplySpec(langle, id1, id2, ids34, rbrace) = self;
        concat((
            langle,
            id1,
            id2.as_ref().map_or(nil(), |(comma, id2)| concat((comma, space(), id2))),
            ids34.as_ref().map_or(nil(), |(vbar, id3, comma, id4)| {
                concat((space(), vbar, space(), id3, comma, space(), id4))
            }),
            rbrace,
        ))
    }
}

impl PrAble for AssignStmt {
    fn pr(&self) -> Pra {
        let AssignStmt(lhs, eq, rhs) = self;
        group((lhs, space(), eq, soft_line(), rhs.pr())).nest(NestAmt(2))
    }
}

impl PrAble for AssignBoolDirStmt {
    fn pr(&self) -> Pra {
        let AssignBoolDirStmt(lhs, (_, dir)) = self;
        concat((lhs, dir))
    }
}

impl PrAble for RecvTypeCast {
    fn pr(&self) -> Pra {
        match self {
            RecvTypeCast::AsInt(kw, lparen, id, rparen) => concat((kw, lparen, id, rparen)),
            RecvTypeCast::AsBool(kw, lparen, id, rparen) => concat((kw, lparen, id, rparen)),
            RecvTypeCast::Ident(id) => id.pr(),
        }
    }
}

impl PrAble for SendStmt {
    fn pr(&self) -> Pra {
        let SendStmt(chan, (_, ctrl1), e1, recv) = self;
        let recv = recv.as_ref().map_or(nil(), |((_, ctrl2), e2)| concat((ctrl2, e2)));
        let e1 = e1.as_ref().map_or(nil(), |v| v.pr());
        concat((chan, ctrl1, e1, recv))
    }
}

impl PrAble for RecvStmt {
    fn pr(&self) -> Pra {
        let RecvStmt(chan, (_, ctrl1), e1, recv) = self;
        let recv = recv.as_ref().map_or(nil(), |((_, ctrl2), e2)| concat((ctrl2, e2)));
        let e1 = e1.as_ref().map_or(nil(), |v| v.pr());
        concat((chan, ctrl1, e1, recv))
    }
}

impl PrAble for GuardedCmd {
    fn pr(&self) -> Pra {
        match self {
            GuardedCmd::Expr(e, arrow, items) => {
                let chunks = items.chunks();
                let items = concat_chunks(line(), chunks, None, NestAmt(2));

                group((e.pr().nest(NestAmt(2)), space(), arrow, space(), items))
            }
            GuardedCmd::Else(kw, arrow, items) => {
                let chunks = items.chunks();
                let items = concat_chunks(line(), chunks, None, NestAmt(2));
                group((kw, space(), arrow, space(), items))
            }
            GuardedCmd::Macro(lparen, ctrl, id, colon1, range, colon2, e, arrow, items, rparen) => {
                let items = concat_chunks(line(), items.chunks(), None, NestAmt(2));
                let prefix = concat((
                    lparen,
                    ctrl,
                    space(),
                    id,
                    space(),
                    colon1,
                    space(),
                    range,
                    space(),
                    colon2,
                ));
                group((prefix, space(), e, space(), arrow, items, space(), rparen))
            }
        }
    }
}

fn format_chp_conditional_like(lbrac: Pra, extra_space: Pra, items: &SepList1<GuardedCmd, Ctrl>, rbrac: Pra) -> Pra {
    if items.items.len() == 1 {
        assert_eq!(items.seps.len(), 0);
        group((lbrac, space(), &items.items[0], line(), rbrac))
    } else {
        let seps = vec![extra_space].into_iter().chain(items.seps.iter().map(|v| v.pr()));
        let chunks = seps
            .zip_eq(items.items.iter())
            .map(|(sep, clause)| concat((sep, space(), clause)).chunk())
            .collect_vec();

        group((lbrac, concat_chunks(nil(), chunks, Some(rbrac), NestAmt(0))))
    }
}

impl PrAble for ChpBracketedStmt {
    fn pr(&self) -> Pra {
        match self {
            ChpBracketedStmt::DetermSelect(lbrac, items, rbrac) => {
                format_chp_conditional_like(lbrac.pr(), space(), items, rbrac.pr())
            }
            ChpBracketedStmt::NonDetermSelect(lbrac, items, rbrac) => {
                format_chp_conditional_like(lbrac.pr(), nil(), items, rbrac.pr())
            }
            ChpBracketedStmt::WhileLoop(lbrac, items, rbrac) => {
                format_chp_conditional_like(lbrac.pr(), space(), items, rbrac.pr())
            }
            ChpBracketedStmt::Wait(lbrac, e, rbrac) => concat((lbrac, space(), e, space(), rbrac)),
            ChpBracketedStmt::DoLoop(lbrac, items, guard, rbrac) => {
                let guard = guard
                    .as_ref()
                    .map_or(nil(), |(arrow, guard)| concat((hard_line(), arrow, space(), guard)));
                group((
                    lbrac,
                    concat_chunks(line(), items.chunks(), None, NestAmt(2)),
                    guard,
                    line(),
                    rbrac,
                ))
            }
        }
    }
}

impl PrAble for ChpStmt {
    fn pr(&self) -> Pra {
        match self {
            ChpStmt::Assign(v) => v.pr(),
            ChpStmt::AssignBoolDir(v) => v.pr(),
            ChpStmt::SendStmt(v) => v.pr(),
            ChpStmt::RecvStmt(v) => v.pr(),
            ChpStmt::Skip(v) => v.pr(),
            ChpStmt::ParenedBody(lparen, items, rparen) => concat((
                lparen,
                concat_chunks(line(), items.chunks(), Some(rparen.pr()), NestAmt(2)),
            )),
            ChpStmt::FuncCall(name, lparen, args, rparen) => concat((
                name,
                lparen,
                group((concat_sep1(args, line()), line_())).nest(NestAmt(2)),
                rparen,
            )),
            ChpStmt::DottedCall(base, dot, id, lparen, args, rparen) => concat((
                base,
                dot,
                id,
                lparen,
                group((concat_sep1(args, line()), line_())).nest(NestAmt(2)),
                rparen,
            )),
            ChpStmt::BracketedStmt(p) => p.pr(),
            ChpStmt::MacroLoop(MacroLoop(lparen, (_, ctrl), id, colon1, range, colon2, items, rparen)) => {
                let items = concat_chunks(line(), items.chunks(), None, NestAmt(2));
                let prefix = concat((
                    lparen,
                    ctrl,
                    space(),
                    id,
                    space(),
                    colon1,
                    space(),
                    range,
                    space(),
                    colon2,
                ));
                group((prefix, space(), items, space(), rparen))
            }
        }
    }
}

impl PrAble for ChpItem {
    fn pr(&self) -> Pra {
        let ChpItem(label, stmt) = self;
        match label {
            Some((label, colon)) => concat((label, colon, space(), space(), stmt)),
            None => stmt.pr(),
        }
    }
}

impl PrChunkListAble for ChpItemList {
    fn chunks(&self) -> Vec<PraChunk> {
        let ChpItemList(l) = self;
        // each item (with terminator) is its own chunk

        let items = l.items.iter().flat_map(|v| v.items.iter());
        let sep_lists = l.items.iter().map(|v| v.seps.iter().map(|v| v.pr()).collect_vec());
        let seps = sep_lists
            .interleave(l.seps.iter().map(|v| vec![v.pr()]))
            .flatten()
            .chain(vec![nil()]);
        items
            .zip_eq(seps)
            .map(|(item, sep)| concat((item, sep)).chunk())
            .collect_vec()
    }
}

impl PrChunkAble for LangChp {
    fn prc(&self) -> PraChunk {
        let LangChp(kw, supply_spec, lbrace, items, rbrace) = self;
        let supply_spec = supply_spec.as_ref().map_or(nil(), |spec| concat((space(), spec)));

        match items {
            Some(items) => {
                let items_and_rbrace = concat_chunks(line(), items.chunks(), Some(rbrace.pr()), NestAmt(2));
                group((kw, supply_spec, space(), lbrace, items_and_rbrace)).chunk()
            }
            None => group((kw, supply_spec, space(), lbrace, rbrace)).chunk(),
        }
    }
}

impl PrChunkListAble for HseBodies {
    fn chunks(&self) -> Vec<PraChunk> {
        match self {
            HseBodies::Body(items) => items.0.chunks(),
            HseBodies::Labeled(labeled) => zip_map2_sep1(labeled, nil(), |labeled_body, semi| {
                let LabeledHseBody(id, colon, items, colon2, id2) = labeled_body;
                let items_and_term =
                    concat_chunks(line(), items.0.chunks(), Some(concat((colon2, id2, semi))), NestAmt(2));
                concat((id, colon, items_and_term)).chunk()
            }),
        }
    }
}

impl PrChunkAble for LangHse {
    fn prc(&self) -> PraChunk {
        let LangHse(kw, supply_spec, lbrace, bodies, rbrace) = self;
        let supply_spec = supply_spec.as_ref().map_or(nil(), |spec| concat((space(), spec)));

        match bodies {
            Some(bodies) => {
                let items_and_rbrace = concat_chunks(line(), bodies.chunks(), Some(rbrace.pr()), NestAmt(2));
                concat((kw, supply_spec, space(), lbrace, items_and_rbrace)).chunk()
            }
            None => concat((kw, supply_spec, space(), lbrace, rbrace)).chunk(),
        }
    }
}

impl PrAble for PrsExpr {
    fn pr(&self) -> Pra {
        match self {
            PrsExpr::Num(v) => v.pr(),
            PrsExpr::ExprId(v) => v.pr(),
            PrsExpr::AtIdent(at, v) => concat((at, v)),
            PrsExpr::Not(op, expr) => group((op, expr.pr())),
            PrsExpr::And(op, e1, e2) => group((e1.pr(), space(), op, line(), e2.pr())),
            PrsExpr::Or(op, e1, e2) => group((e1.pr(), space(), op, line(), e2.pr())),
            PrsExpr::MacroLoop(MacroLoop(lparen, (_, ctrl), id, colon1, range, colon2, e, rparen)) => concat((
                lparen,
                ctrl,
                space(),
                id,
                colon1,
                space(),
                range,
                colon2,
                space(),
                e.pr(),
                rparen,
            )),
            PrsExpr::Parened(lparen, e, rparen) => group((lparen, e.pr(), rparen)),
            PrsExpr::Sized(lhs, langle, es, rangle) => group((lhs.pr(), langle, concat_sep1(es, space()), rangle)),
            PrsExpr::Braced(lbrace, (_, dir), e, rbrace, lhs) => group((lbrace, dir, e.pr(), rbrace, lhs.pr())),
        }
    }
}

impl PrAble for TreeSubcktSpec {
    fn pr(&self) -> Pra {
        match self {
            TreeSubcktSpec::Expr(e) => e.pr(),
            TreeSubcktSpec::Str(s) => s.pr(),
        }
    }
}

impl PrAble for SizeSpec {
    fn pr(&self) -> Pra {
        let SizeSpec(langle, e, opt, opt2, rangle) = self;

        let opt = opt.as_ref().map_or(nil(), |(comma, e, v)| {
            let v = v.as_ref().map_or(nil(), |(comma, e)| concat((comma, space(), e)));
            concat((comma, space(), e, v))
        });
        let opt2 = opt2.as_ref().map_or(nil(), |(semi, e)| concat((semi, space(), e)));
        concat((langle, e, opt, opt2, rangle))
    }
}

impl PrAble for PrsItem {
    fn pr(&self) -> Pra {
        match self {
            PrsItem::Rule(lhs, (_, arrow), rhs, (_, dir)) => concat((lhs, space(), arrow, space(), rhs, dir)),
            PrsItem::AtRule(lhs, (_, arrow), at_sign, rhs, (_, dir)) => {
                concat((lhs, space(), arrow, at_sign, space(), rhs, dir))
            }
            PrsItem::SubBlock(id, spec, lbrace, body, rbrace) => {
                let spec = spec
                    .as_ref()
                    .map_or(nil(), |(langle, spec, rangle)| concat((langle, spec, rangle)));
                let body_with_rbrace = concat_chunks(line(), body.chunks(), Some(rbrace.pr()), NestAmt(2));
                concat((id, spec, space(), lbrace, body_with_rbrace))
            }
            PrsItem::MacroLoop(MacroLoop(lparen, (), id, colon1, range, colon2, items, rparen)) => {
                let items = concat_chunks(line(), items.chunks(), None, NestAmt(2));
                let prefix = concat((lparen, space(), id, space(), colon1, space(), range, space(), colon2));
                group((prefix, space(), items, space(), rparen))
            }
            PrsItem::Pass((_, kw), size_spec, lparen, id1, comma1, id2, comma2, id3, rparen) => {
                let size_spec = size_spec.as_ref().map_or(nil(), |size_spec| size_spec.pr());
                concat((
                    kw,
                    space(),
                    size_spec,
                    lparen,
                    id1,
                    comma1,
                    space(),
                    id2,
                    comma2,
                    space(),
                    id3,
                    rparen,
                ))
            }
            PrsItem::TransGate(kw, size_spec, lparen, id1, comma1, id2, comma2, id3, comma3, id4, rparen) => {
                let size_spec = size_spec.as_ref().map_or(nil(), |size_spec| size_spec.pr());
                concat((
                    kw,
                    space(),
                    size_spec,
                    lparen,
                    id1,
                    comma1,
                    space(),
                    id2,
                    comma2,
                    space(),
                    id3,
                    comma3,
                    space(),
                    id4,
                    rparen,
                ))
            }
        }
    }
}

impl PrChunkAble for PrsBodyRow {
    fn prc(&self) -> PraChunk {
        let PrsBodyRow(attr_list, item) = self;
        let attr_list = attr_list.as_ref().map_or(nil(), |v| concat((v, space())));
        concat((attr_list, item)).chunk()
    }
}

impl PrChunkListAble for PrsBody {
    fn chunks(&self) -> Vec<PraChunk> {
        let PrsBody(body_rows) = self;
        body_rows.iter().map(|v| v.prc()).collect_vec()
    }
}

impl PrChunkAble for LangPrs {
    fn prc(&self) -> PraChunk {
        let LangPrs(kw, supply_spec, opt_star, lbrace, body, rbrace) = self;
        let supply_spec = supply_spec.as_ref().map_or(nil(), |spec| concat((space(), spec)));
        let opt_star = opt_star.as_ref().map_or(nil(), |star| concat((space(), star)));
        let chunks = body.chunks();
        let body_and_rbrace = match chunks.len() {
            0 => rbrace.pr(),
            _ => concat_chunks(line(), body.chunks(), Some(rbrace.pr()), NestAmt(2)),
        };
        concat((kw, supply_spec, opt_star, space(), lbrace, body_and_rbrace)).chunk()
    }
}

impl PrAble for TimingBodyClause {
    fn pr(&self) -> Pra {
        let TimingBodyClause(e, star, ctrl) = self;
        let star = star.as_ref().map_or(nil(), |v| v.pr());
        let ctrl = ctrl.as_ref().map_or(nil(), |(_, v)| v.pr());
        concat((e, star, ctrl))
    }
}

impl PrAble for TimingBody {
    fn pr(&self) -> Pra {
        let TimingBody(tc1, opt_qmark, tc2, (_, tt), e, tc3) = self;
        let opt_qmark = opt_qmark.as_ref().map_or(nil(), |v| v.pr());
        let qmark_and_tc2 = match tc2 {
            None => opt_qmark,
            Some((colon, tc2)) => concat((colon, opt_qmark, space(), tc2)),
        };
        let e = e.as_ref().map_or(nil(), |(lbrac, e, rbrac)| concat((lbrac, e, rbrac)));
        concat((tc1, qmark_and_tc2, tt, e, tc3))
    }
}

impl PrChunkAble for SpecItem {
    fn prc(&self) -> PraChunk {
        match self {
            SpecItem::Normal(id, lparen, eids, rparen) => concat((
                id,
                lparen,
                concat((concat_sep1(eids, line()), line_())).group().nest(NestAmt(2)),
                rparen,
            ))
                .chunk(),
            SpecItem::Timing(kw, body) => concat((kw, body)).chunk(),
        }
    }
}

impl PrAble for SpecBody {
    fn pr(&self) -> Pra {
        let SpecBody {
            lbrace,
            requires_clause,
            ensures_clause,
            generic_clause,
            rbrace,
        } = self;

        let mut chunks = Vec::new();

        for c in [requires_clause, ensures_clause] {
            match c {
                None => {}
                Some((kw, (lbrace, items, rbrace))) => {
                    let items_and_rbrace = concat_chunks(
                        line(),
                        items.iter().map(|v| v.prc()).collect_vec(),
                        Some(rbrace.pr()),
                        NestAmt(2),
                    );
                    chunks.push(concat((kw, space(), lbrace, items_and_rbrace)).chunk());
                }
            }
        }

        generic_clause.iter().for_each(|v| chunks.push(v.prc()));

        concat((lbrace, concat_chunks(line(), chunks, Some(rbrace.pr()), NestAmt(2))))
    }
}

impl PrChunkAble for LangSpec {
    fn prc(&self) -> PraChunk {
        let LangSpec(kw, body) = self;
        concat((kw, space(), body)).chunk()
    }
}

impl PrChunkAble for LangRefine {
    fn prc(&self) -> PraChunk {
        let LangRefine(kw, lbrace, items, rbrace) = self;
        concat((
            kw,
            lbrace,
            concat_chunks(
                line(),
                items.iter().map(|v| v.prc()).collect_vec(),
                Some(rbrace.pr()),
                NestAmt(2),
            ),
        ))
            .chunk()
    }
}

impl PrAble for DirectivePart {
    fn pr(&self) -> Pra {
        let DirectivePart((_, dir), e, o) = self;
        let o = o.as_ref().map_or(nil(), |(comma, (id, o))| {
            let o = o.as_ref().map_or(nil(), |(comma, e)| concat((comma, space(), e)));
            concat((comma, space(), id, o))
        });
        concat((dir, space(), e, o))
    }
}

impl PrAble for SizingItem {
    fn pr(&self) -> Pra {
        match self {
            SizingItem::Setup(id, arrow, e) => concat((id, space(), arrow, space(), e)),
            SizingItem::Directive(id, lbrace, dir_part, dir_part2, rbrace) => {
                let dir_part2 = dir_part2.as_ref().map_or(nil(), |(semi, v)| concat((semi, space(), v)));
                concat((id, lbrace, dir_part, dir_part2, rbrace))
            }
            SizingItem::DirectiveMacroLoop(MacroLoop(lparen, semi, id, colon1, range, colon2, items, rparen)) => {
                let chunks_then_close_paren =
                    concat_chunks(line(), zip_sep1_as_chunks(items, nil()), Some(rparen.pr()), NestAmt(2));
                group((
                    lparen,
                    semi,
                    space(),
                    id,
                    space(),
                    colon1,
                    space(),
                    range,
                    colon2,
                    chunks_then_close_paren,
                ))
            }
        }
    }
}

impl PrChunkAble for LangSizing {
    fn prc(&self) -> PraChunk {
        let LangSizing(kw, lbrace, items, rbrace) = self;
        let chunks_and_rbrace = concat_chunks(line(), zip_sep1_as_chunks(items, nil()), Some(rbrace.pr()), NestAmt(2));
        concat((kw, lbrace, chunks_and_rbrace)).chunk()
    }
}

impl PrAble for ActionItem {
    fn pr(&self) -> Pra {
        let ActionItem(id, lbrace, items, rbrace) = self;
        let chunks_and_rbrace = concat_chunks(line(), items.0.chunks(), Some(rbrace.pr()), NestAmt(2));
        concat((id, lbrace, chunks_and_rbrace))
    }
}

impl PrChunkAble for LangInitialize {
    fn prc(&self) -> PraChunk {
        let LangInitialize(kw, lbrace, items, rbrace) = self;
        let chunks_and_rbrace = concat_chunks(line(), zip_sep1_as_chunks(items, nil()), Some(rbrace.pr()), NestAmt(2));
        concat((kw, lbrace, chunks_and_rbrace)).chunk()
    }
}

impl PrChunkAble for DataflowOrdering {
    fn prc(&self) -> PraChunk {
        let DataflowOrdering(id, lbrace, items, rbrace) = self;
        let items = zip_map_sep1_as_chunks(items, nil(), |(ids1, c, ids2)| {
            concat((
                concat_sep1(ids1, space()),
                space(),
                c,
                space(),
                concat_sep1(ids2, space()),
            ))
        });
        let chunks_and_rbrace = concat_chunks(line(), items, Some(rbrace.pr()), NestAmt(2));
        concat((id, lbrace, chunks_and_rbrace)).chunk()
    }
}

impl PrAble for ExprIdOrStar {
    fn pr(&self) -> Pra {
        match self {
            ExprIdOrStar::ExprId(v) => v.pr(),
            ExprIdOrStar::Star(v) => v.pr(),
        }
    }
}

impl PrAble for ExprIdOrStarOrBar {
    fn pr(&self) -> Pra {
        match self {
            ExprIdOrStarOrBar::ExprId(v) => v.pr(),
            ExprIdOrStarOrBar::Star(v) => v.pr(),
            ExprIdOrStarOrBar::Bar(v) => v.pr(),
        }
    }
}

impl PrAble for DataflowItem {
    fn pr(&self) -> Pra {
        match self {
            DataflowItem::BracketedOrParenedFlow(e1, arrow, brac, id) => {
                let brac = brac.as_ref().map_or(nil(), |(_, lbrac, e1, e2, rbrac)| {
                    let e2 = e2.as_ref().map_or(nil(), |(c, e2)| concat((c, space(), e2)));
                    concat((lbrac, e1, e2, rbrac))
                });
                concat((e1, space(), arrow, brac, space(), id))
            }
            DataflowItem::BracedFlow(lbrace, c1, rbrace, ids1, arrow, ids2) => concat((
                lbrace,
                c1,
                rbrace,
                space(),
                concat_sep1(ids1, space()),
                space(),
                arrow,
                space(),
                concat_sep1(ids2, space()),
            )),
            DataflowItem::Cluster(kw, lbrace, items, rbrace) => concat((
                kw,
                lbrace,
                concat_chunks(line(), zip_sep1_as_chunks(items, nil()), Some(rbrace.pr()), NestAmt(2)),
            )),
            DataflowItem::Sink(id, arrow, star) => concat((id, space(), arrow, space(), star)),
        }
    }
}

impl PrChunkAble for LangDataflow {
    fn prc(&self) -> PraChunk {
        let LangDataflow(kw, lbrace, ordering, items, rbrace) = self;
        let ordering = match ordering {
            Some(ordering) => vec![ordering.prc()],
            None => vec![],
        };
        let items = zip_sep1_as_chunks(items, nil());
        let chunks = ordering.into_iter().chain(items.into_iter()).collect_vec();
        concat((kw, lbrace, concat_chunks(line(), chunks, Some(rbrace.pr()), NestAmt(2)))).chunk()
    }
}

impl PrChunkAble for TopItem {
    fn prc(&self) -> PraChunk {
        match self {
            TopItem::Instance(v) => v.prc(),
            TopItem::Connection(v) => v.prc(),
            TopItem::Alias(v) => v.prc(),
            TopItem::DynamicLoop(v) => v.prc(),
            TopItem::MacroLoop(v) => v.prc(),
            TopItem::Conditional(v) => v.prc(),
            TopItem::Assertion(v) => v.prc(),
            TopItem::DebugOutput(v) => v.prc(),
            TopItem::Chp(v) => v.prc(),
            TopItem::Hse(v) => v.prc(),
            TopItem::Prs(v) => v.prc(),
            TopItem::Spec(v) => v.prc(),
            TopItem::Refine(v) => v.prc(),
            TopItem::Sizing(v) => v.prc(),
            TopItem::Initialize(v) => v.prc(),
            TopItem::Dataflow(v) => v.prc(),
            TopItem::Namespace(v) => v.prc(),
            TopItem::Import(kw_import, import, semi) => concat((kw_import, space(), import, semi)).chunk(),
            TopItem::Open(kw_opt, name, rename, semi) => {
                let rename = rename.map_or(nil(), |(arrow, id)| concat((space(), arrow, space(), id)));
                concat((kw_opt, space(), name, rename, semi)).chunk()
            }
            TopItem::DefTemplated(spec, def, body) => group((spec, line_(), def, body)).chunk(),
            TopItem::DefEnum(v) => v.prc(),
        }
    }
}

impl PrChunkAble for MethodsBody {
    fn prc(&self) -> PraChunk {
        let MethodsBody(kw, lbrace, methods, rbrace) = self;
        match methods.len() {
            0 => group((
                kw,
                space(),
                lbrace,
                hard_line(),
                concat_vec(methods, hard_line()),
                rbrace,
            ))
                .chunk(),
            _ => group((kw, space(), lbrace, rbrace)).chunk(),
        }
    }
}

impl PrAble for Method {
    fn pr(&self) -> Pra {
        match self {
            Method::Hse(id, lbrace, items, rbrace) => concat((
                id,
                lbrace,
                concat_chunks(line(), items.0.chunks(), Some(rbrace.pr()), NestAmt(2)),
            )),
            Method::Assign(id, eq, e, semi) => concat((id, space(), eq, space(), e, semi)),
            Method::Macro(kw, id, ports, lbrace, items, rbrace) => {
                let body = match items {
                    Some(items) => concat((
                        lbrace,
                        concat_chunks(line(), items.0.chunks(), Some(rbrace.pr()), NestAmt(2)),
                    )),
                    None => concat((lbrace, rbrace)),
                };
                concat((kw, space(), id, ports, body))
            }
        }
    }
}

impl PrAble for ProclikeBody {
    fn pr(&self) -> Pra {
        match self {
            ProclikeBody::NoBody(semi) => semi.pr(),
            ProclikeBody::WithBody(overrides, lbrace, items, methods, rbrace) => {
                let mut items = items.iter().map(|v| v.prc()).collect_vec();
                match methods {
                    Some(methods) => items.push(methods.prc()),
                    None => {}
                }

                let items = match items.len() {
                    0 => rbrace.pr(),
                    _ => concat_chunks(hard_line(), items, Some(rbrace.pr()), NestAmt(2)),
                };

                group((space(), overrides.as_ref().map_or(nil(), |v| v.pr()), lbrace, items))
            }
        }
    }
}

impl PrChunkAble for NamespaceDecl {
    fn prc(&self) -> PraChunk {
        let NamespaceDecl(opt_kw_export, kw_namespace, name, lbrace, items, rbrace) = self;
        let kw_export = opt_kw_export.map_or(nil(), |kw| concat((kw, space())));
        let chunks = items.iter().map(|v| v.prc()).collect_vec();

        let items = match items.len() {
            0 => rbrace.pr(),
            _ => concat_chunks(line(), chunks, Some(rbrace.pr()), NestAmt(2)),
        };

        group((kw_export, kw_namespace, space(), name, space(), lbrace, items)).chunk()
    }
}

impl PrAble for Import {
    fn pr(&self) -> Pra {
        match self {
            Import::String(s) => s.pr(),
            Import::Namespace(name, val) => {
                let name = name.pr();
                match val {
                    Some((arrow, id)) => group((name, space(), arrow.pr(), line(), id)),
                    None => name,
                }
            }
            Import::Ident(id, arrow, id2) => group((id, space(), arrow, line(), id2)),
        }
    }
}

pub fn print_pretty(
    ast: &ParseTree,
    width: usize,
) -> Option<String> {
    let chunks = ast.ast.iter().map(|v| v.prc()).collect_vec();
    let chunks = concat_chunks(line(), chunks, None, NestAmt(0));
    as_pretty(chunks, &ast.final_comments, &ast.flat_tokens, &ast.tokens, width)
}
