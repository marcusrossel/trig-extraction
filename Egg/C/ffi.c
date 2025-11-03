#include <lean/lean.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct env {
    void* x1;
    void* x2;
    void* x3;
    void* x4;
    void* x5;
} env;

typedef struct rw_rule {
    const char* name;
    const char* lhs;
    const char* rhs;
} rw_rule;

rw_rule rw_rule_from_lean_obj(lean_obj_arg rw) {
    return (rw_rule) { 
        .name = lean_string_cstr(lean_ctor_get(rw, 0)),
        .lhs  = lean_string_cstr(lean_ctor_get(rw, 1)),
        .rhs  = lean_string_cstr(lean_ctor_get(rw, 2))
    };
}

typedef struct rw_rule_array {
    rw_rule* ptr;
    size_t   len;
} rw_rule_array;

rw_rule_array rw_rule_array_from_lean_obj(lean_obj_arg rws) {
    lean_object** rws_c_ptr = lean_array_cptr(rws);
    size_t rws_count = lean_array_size(rws);
    rw_rule* c_rws = malloc(rws_count * sizeof(rw_rule));
    for (int idx = 0; idx < rws_count; idx++) {
        c_rws[idx] = rw_rule_from_lean_obj(rws_c_ptr[idx]);
    }
    return (rw_rule_array) { .ptr = c_rws, .len = rws_count };
}

typedef void* egraph;

extern void free_egraph(egraph);

void egraph_finalize(egraph obj) {
    free_egraph(obj);
}

void egraph_foreach(egraph _x, b_lean_obj_arg _y) {
    // do nothing since `egraph` does not contain nested Lean objects
}

static lean_external_class* egraph_class = NULL;

lean_object* egraph_to_lean(egraph e) {
    if (egraph_class == NULL) {
        egraph_class = lean_register_external_class(egraph_finalize, egraph_foreach);
    }
    return lean_alloc_external(egraph_class, e);
}

egraph to_egraph(b_lean_obj_arg e) {
    return (egraph)(lean_get_external_data(e));
}

typedef struct egg_result {
    uint8_t success;
    char* term;
    egraph graph;
} egg_result;

lean_obj_res egg_result_to_lean(egg_result result) {
    lean_object* term  = lean_mk_string(result.term);
    lean_object* graph = egraph_to_lean(result.graph);

    lean_object* lean_result = lean_alloc_ctor(0, 2, sizeof(uint8_t));
    lean_ctor_set(lean_result, 0, term);
    
    unsigned scalar_base_offset = lean_ctor_num_objs(lean_result) * sizeof(void*);
    lean_ctor_set_uint8(lean_result, scalar_base_offset + 0, result.success);

    if (graph == NULL) {
        lean_object* option_nil = lean_alloc_ctor(0, 0, 0); // Option.nil
        lean_ctor_set(lean_result, 1, option_nil);
    } else {
        lean_object* some_graph = lean_alloc_ctor(1, 1, 0); // Option.some
        lean_ctor_set(some_graph, 0, graph);
        lean_ctor_set(lean_result, 1, some_graph);
    }

    return lean_result;
}

extern egg_result run_egg(const char* target, rw_rule_array rws, void* e);

lean_obj_res run_egg_c(lean_obj_arg target, lean_obj_arg rw_rules, lean_obj_arg x1, lean_obj_arg x2, lean_obj_arg x3, lean_obj_arg x4, lean_obj_arg x5) {
    env e = { .x1 = x1, .x2 = x2, .x3 = x3, .x4 = x4, .x5 = x5 };
    const char* tgt = lean_string_cstr(target);
    rw_rule_array rws = rw_rule_array_from_lean_obj(rw_rules);
    
    egg_result res = run_egg(tgt, rws, &e);
    
    lean_obj_res result = egg_result_to_lean(res);
    lean_object* metam_state = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(metam_state, 0, result);
    lean_ctor_set(metam_state, 1, x5);

    // TODO: Is it safe to free this?
    free(rws.ptr);

    return metam_state;
}

extern egg_result query_egraph(egraph graph, const char* query);

lean_obj_res query_egraph_c(b_lean_obj_arg g, lean_obj_arg q) {
    const char* query = lean_string_cstr(q);
    egraph graph = to_egraph(g);
    egg_result result = query_egraph(graph, query);
    return egg_result_to_lean(result);    
}