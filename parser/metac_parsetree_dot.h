#ifndef METAC_PARSETREE_DOT_H
#define METAC_PARSETREE_DOT_H

// Debug helper: writes a parse tree as a graphviz digraph.

typedef struct metac_dot_ctx_t
{
    FILE* Out;
    const metac_identifier_table_t* Ids;
    const metac_identifier_table_t* Strings;
    uint32_t NextId;
} metac_dot_ctx_t;

static bool DotIsAbsent(const void* p)
{
    return !p || p == emptyPointer;
}

static void DotEscape(const char* src, char* dst, size_t cap)
{
    size_t n = 0;
    if (!src)
        src = "";

    while (*src && n + 2 < cap)
    {
        char ch = *src++;
        if (ch == '"' || ch == '\\')
            dst[n++] = '\\';
        dst[n++] = (ch == '\n' || ch == '\r') ? ' ' : ch;
    }
    dst[n] = 0;
}

static uint32_t DotNode(metac_dot_ctx_t* c, const char* kind, const char* text)
{
    char escKind[128];
    char escText[256];
    uint32_t id = c->NextId++;

    DotEscape(kind, escKind, sizeof(escKind));
    if (text && *text)
    {
        DotEscape(text, escText, sizeof(escText));
        fprintf(c->Out, "  n%u [label=\"%s\\n%s\"];\n", id, escKind, escText);
    }
    else
    {
        fprintf(c->Out, "  n%u [label=\"%s\"];\n", id, escKind);
    }
    return id;
}

static void DotEdge(metac_dot_ctx_t* c, uint32_t parent, uint32_t child, const char* label)
{
    if (parent && child)
        fprintf(c->Out, "  n%u -> n%u [label=\"%s\"];\n", parent, child, label);
}

static const char* DotIdText(const metac_identifier_table_t* table, metac_identifier_ptr_t ptr)
{
    if (!ptr.v || ptr.v == empty_identifier.v)
        return "";
    return IdentifierPtrToCharPtr(table, ptr);
}

static uint32_t DotExpr(metac_dot_ctx_t* c, metac_expr_t* e);
static uint32_t DotType(metac_dot_ctx_t* c, decl_type_t* t);
static uint32_t DotDecl(metac_dot_ctx_t* c, metac_decl_t* d);
static uint32_t DotStmt(metac_dot_ctx_t* c, metac_stmt_t* s);

static uint32_t DotExpr(metac_dot_ctx_t* c, metac_expr_t* e)
{
    uint32_t id = 0;
    uint32_t child = 0;
    const char* kname;
    char numText[64];

    if (DotIsAbsent(e))
        return 0;

    kname = MetaCExprKind_toChars((metac_expr_kind_t)e->Kind);

    switch (e->Kind)
    {
        case expr_identifier:
        {
            id = DotNode(c, kname, DotIdText(c->Ids, e->IdentifierPtr));
        } break;

        case expr_string:
        {
            id = DotNode(c, kname, DotIdText(c->Strings, e->StringPtr));
        } break;

        case expr_char:
        {
            id = DotNode(c, kname, e->Chars);
        } break;

        case expr_signed_integer:
        {
            snprintf(numText, sizeof(numText), "%lld", (long long)e->ValueI64);
            id = DotNode(c, kname, numText);
        } break;

        case expr_type:
        {
            id = DotNode(c, kname, "");
            child = DotType(c, e->TypeExp);
            DotEdge(c, id, child, "type");
        } break;

        case expr_argument:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, e->Expr);
            DotEdge(c, id, child, "expr");
            if (!DotIsAbsent(e->Next))
            {
                child = DotExpr(c, (metac_expr_t*)e->Next);
                DotEdge(c, id, child, "next");
            }
        } break;

        case expr_call:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, e->E1);
            DotEdge(c, id, child, "callee");
            child = DotExpr(c, e->E2);
            DotEdge(c, id, child, "args");
        } break;

        case expr_ternary:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, e->E1);
            DotEdge(c, id, child, "cond");
            child = DotExpr(c, e->E2);
            DotEdge(c, id, child, "then");
            child = DotExpr(c, e->Econd);
            DotEdge(c, id, child, "else");
        } break;

        // binary: E1 is the left operand, E2 the right operand
        case expr_index:
        case expr_template_instance:
        case expr_comma:
        case expr_dot:
        case expr_add: case expr_sub: case expr_mul: case expr_div: case expr_rem:
        case expr_xor: case expr_or: case expr_and: case expr_lsh: case expr_rsh:
        case expr_oror: case expr_andand:
        case expr_arrow: case expr_dotdot: case expr_assign:
        case expr_add_ass: case expr_sub_ass: case expr_mul_ass: case expr_div_ass:
        case expr_rem_ass: case expr_xor_ass: case expr_or_ass: case expr_and_ass:
        case expr_lsh_ass: case expr_rsh_ass:
        case expr_eq: case expr_neq: case expr_lt: case expr_le: case expr_gt: case expr_ge:
        case expr_spaceship:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, e->E1);
            DotEdge(c, id, child, "lhs");
            child = DotExpr(c, e->E2);
            DotEdge(c, id, child, "rhs");
        } break;

        // unary: E1 is the operand
        case expr_increment: case expr_decrement: case expr_post_increment:
        case expr_post_decrement: case expr_sizeof: case expr_inject: case expr_eject:
        case expr_assert: case expr_outer: case expr_stringize: case expr_unary_dot:
        case expr_addr: case expr_deref: case expr_not: case expr_compl:
        case expr_uplus: case expr_umin: case expr_paren: case expr_run:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, e->E1);
            DotEdge(c, id, child, "operand");
        } break;

        case expr_cast:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, e->CastExp);
            DotEdge(c, id, child, "operand");
        } break;

        default:
        {
            id = DotNode(c, kname, "");
        } break;
    }

    return id;
}

static uint32_t DotType(metac_dot_ctx_t* c, decl_type_t* t)
{
    uint32_t id;
    uint32_t child;
    const char* kname;
    char text[64];

    if (DotIsAbsent(t))
        return 0;

    kname = MetaCNodeKind_toChars((metac_node_kind_t)t->Kind);

    if (t->TypeKind == type_identifier)
    {
        id = DotNode(c, kname, DotIdText(c->Ids, t->TypeIdentifier));
    }
    else
    {
        snprintf(text, sizeof(text), "typekind=%d", (int)t->TypeKind);
        id = DotNode(c, kname, text);
    }

    if (t->Kind == decl_type_ptr)
    {
        child = DotType(c, ((decl_type_ptr_t*)t)->ElementType);
        DotEdge(c, id, child, "element");
    }

    return id;
}

static uint32_t DotVariable(metac_dot_ctx_t* c, decl_variable_t* v)
{
    uint32_t id;
    uint32_t child;

    if (DotIsAbsent(v))
        return 0;

    id = DotNode(c, "decl_variable", DotIdText(c->Ids, v->VarIdentifier));
    child = DotType(c, v->VarType);
    DotEdge(c, id, child, "type");
    child = DotExpr(c, v->VarInitExpr);
    DotEdge(c, id, child, "init");
    return id;
}

static void DotParamList(metac_dot_ctx_t* c, uint32_t parent, decl_parameter_t* p, const char* label)
{
    while (!DotIsAbsent(p))
    {
        uint32_t child = DotVariable(c, p->Parameter);
        DotEdge(c, parent, child, label);
        p = p->Next;
    }
}

static void DotStmtList(metac_dot_ctx_t* c, uint32_t parent, metac_stmt_t* s, const char* label)
{
    while (!DotIsAbsent(s))
    {
        uint32_t child = DotStmt(c, s);
        DotEdge(c, parent, child, label);
        s = s->Next;
    }
}

static uint32_t DotDecl(metac_dot_ctx_t* c, metac_decl_t* d)
{
    uint32_t id = 0;
    uint32_t child = 0;
    const char* kname;

    if (DotIsAbsent(d))
        return 0;

    kname = MetaCNodeKind_toChars((metac_node_kind_t)d->Kind);

    switch (d->Kind)
    {
        case decl_function:
        {
            decl_function_t* f = &d->decl_function;
            id = DotNode(c, kname, DotIdText(c->Ids, f->Identifier));
            child = DotType(c, f->ReturnType);
            DotEdge(c, id, child, "return");
            DotParamList(c, id, f->TemplateParameters, "template");
            DotParamList(c, id, f->FunctionParameters, "param");
            child = DotStmt(c, (metac_stmt_t*)f->FunctionBody);
            DotEdge(c, id, child, "body");
        } break;

        case decl_variable:
        {
            id = DotVariable(c, &d->decl_variable);
        } break;

        case decl_parameter:
        {
            id = DotVariable(c, d->decl_parameter.Parameter);
        } break;

        case decl_enum_member:
        {
            decl_enum_member_t* m = &d->decl_enum_member;
            id = DotNode(c, kname, DotIdText(c->Ids, m->Identifier));
            child = DotExpr(c, m->Value);
            DotEdge(c, id, child, "value");
        } break;

        case decl_label:
        {
            id = DotNode(c, kname, DotIdText(c->Ids, d->decl_label.Identifier));
            child = DotDecl(c, (metac_decl_t*)d->decl_label.Decl);
            DotEdge(c, id, child, "decl");
        } break;

        case decl_type_struct:
        case decl_type_union:
        case decl_type_enum:
        case decl_type_array:
        case decl_type_ptr:
        case decl_type_functiontype:
        case decl_type_typeof:
        case decl_type_tuple:
        case decl_type_template_instance:
        case decl_type_typedef:
        {
            id = DotType(c, (decl_type_t*)d);
        } break;

        default:
        {
            id = DotNode(c, kname, "");
        } break;
    }

    return id;
}

static uint32_t DotStmt(metac_dot_ctx_t* c, metac_stmt_t* s)
{
    uint32_t id = 0;
    uint32_t child = 0;
    const char* kname;

    if (DotIsAbsent(s))
        return 0;

    kname = MetaCNodeKind_toChars((metac_node_kind_t)s->Kind);

    switch (s->Kind)
    {
        case stmt_block:
        {
            id = DotNode(c, kname, "");
            DotStmtList(c, id, s->stmt_block.Body, "stmt");
        } break;

        case stmt_if:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_if.IfCond);
            DotEdge(c, id, child, "cond");
            child = DotStmt(c, s->stmt_if.IfBody);
            DotEdge(c, id, child, "then");
            child = DotStmt(c, s->stmt_if.ElseBody);
            DotEdge(c, id, child, "else");
        } break;

        case stmt_while:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_while.WhileExp);
            DotEdge(c, id, child, "cond");
            child = DotStmt(c, s->stmt_while.WhileBody);
            DotEdge(c, id, child, "body");
        } break;

        case stmt_do_while:
        {
            id = DotNode(c, kname, "");
            child = DotStmt(c, s->stmt_do_while.DoWhileBody);
            DotEdge(c, id, child, "body");
            child = DotExpr(c, s->stmt_do_while.DoWhileExp);
            DotEdge(c, id, child, "cond");
        } break;

        case stmt_for:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_for.ForCond);
            DotEdge(c, id, child, "cond");
            child = DotExpr(c, s->stmt_for.ForPostLoop);
            DotEdge(c, id, child, "post");
            child = DotStmt(c, s->stmt_for.ForBody);
            DotEdge(c, id, child, "body");
        } break;

        case stmt_switch:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_switch.SwitchExp);
            DotEdge(c, id, child, "cond");
            child = DotStmt(c, (metac_stmt_t*)s->stmt_switch.SwitchBody);
            DotEdge(c, id, child, "body");
        } break;

        case stmt_case:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_case.CaseExp);
            DotEdge(c, id, child, "exp");
            child = DotStmt(c, s->stmt_case.CaseBody);
            DotEdge(c, id, child, "body");
        } break;

        case stmt_return:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_return.ReturnExp);
            DotEdge(c, id, child, "exp");
        } break;

        case stmt_yield:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_yield.YieldExp);
            DotEdge(c, id, child, "exp");
        } break;

        case stmt_expr:
        {
            id = DotNode(c, kname, "");
            child = DotExpr(c, s->stmt_expr.Expr);
            DotEdge(c, id, child, "exp");
        } break;

        case stmt_decl:
        {
            id = DotNode(c, kname, "");
            child = DotDecl(c, s->stmt_decl.Decl);
            DotEdge(c, id, child, "decl");
        } break;

        case stmt_run:
        {
            id = DotNode(c, kname, "");
            child = DotStmt(c, s->stmt_run.RunBody);
            DotEdge(c, id, child, "body");
        } break;

        case stmt_scope:
        {
            id = DotNode(c, kname, "");
            child = DotStmt(c, s->stmt_scope.Stmt);
            DotEdge(c, id, child, "stmt");
        } break;

        case stmt_label:
        {
            id = DotNode(c, kname, DotIdText(c->Ids, s->stmt_label.Label));
        } break;

        case stmt_goto:
        {
            id = DotNode(c, kname, DotIdText(c->Ids, s->stmt_goto.GotoLabel));
        } break;

        default:
        {
            id = DotNode(c, kname, "");
        } break;
    }

    return id;
}

static void MetaCParseTree_DumpDot(FILE* out, decl_array_t decls,
                                   const metac_identifier_table_t* ids,
                                   const metac_identifier_table_t* strings)
{
    metac_dot_ctx_t ctx;
    uint32_t root;

    ctx.Out = out;
    ctx.Ids = ids;
    ctx.Strings = strings;
    ctx.NextId = 1;

    fprintf(out, "digraph parsetree {\n");
    fprintf(out, "  node [shape=box, fontname=\"Consolas\"];\n");

    root = DotNode(&ctx, "parse tree", "");
    for (uint32_t i = 0; i < decls.Length; i++)
    {
        uint32_t child = DotDecl(&ctx, decls.Ptr[i]);
        DotEdge(&ctx, root, child, "decl");
    }

    fprintf(out, "}\n");
}

#endif
