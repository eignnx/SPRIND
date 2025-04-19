:- module(optree_dotgraph, [
    print_dottree/1,
    print_dottrees/0
]).


dotprint_tree(Fmt, Tree) :-
    format('digraph "Format ~k" {~n', [Fmt]),
    format('  graph [dpi = 100, bgcolor="#111", fontcolor="white", rankdir=LR, pad="0.25"];~n'),
    format('  node [fontname = "Courier", fontsize="15pt", color="white", fontcolor="white"];~n'),
    format('  edge [fontname = "Courier", color="white", fontcolor="white"];~n'),
    ( Tree \= empty -> dotprint_tree_(Tree, ``) ; format('// Empty Tree~n') ),
    format('}~n').


dotprint_tree_(leaf(Instr), Prefix) :-
    node_id_label(leaf(Instr), Id, _),
    instr_info(Instr, Info),
    length(Prefix, PLen),
    format(atom(Label), '~k\\n~w\\n0b~s`~d', [Instr, Info.title, Prefix, PLen]),
    format('  ~w [label = "~w", shape = rectangle];~n', [Id, Label]).
dotprint_tree_(subtree(Split, Tree), Prefix) :-
    node_id_label(subtree(Split, Tree), Id, Label),
    node_id_label(Tree, TreeId, _),
    format('  ~w [label = ~w, shape = ellipse];~n', [Id, Label]),
    format('  ~w:e -> ~w:w;~n', [Id, TreeId]),
    dotprint_tree_(Tree, Prefix).
dotprint_tree_(node(Left, Split, Right), Prefix) :-
    node_id_label(node(Left, Split, Right), Id, Label),
    node_id_label(Left, LeftId, _),
    node_id_label(Right, RightId, _),
    format('  ~w [label = ~w, shape = ellipse];~n', [Id, Label]),
    format('  ~w:e -> ~w:w [label = "1"];~n', [Id, RightId]),
    format('  ~w:e -> ~w:w [label = "0"];~n', [Id, LeftId]),
    append(Prefix, `0`, LeftPrefix),
    append(Prefix, `1`, RightPrefix),
    dotprint_tree_(Right, RightPrefix),
    dotprint_tree_(Left, LeftPrefix).


node_id_label(leaf(Instr), Id, Label) :-
    term_hash(leaf(Instr), Hash),
    format(atom(Id), '"~k_~d"', [Instr, Hash]),
    format(atom(Label), '"~k"', [Instr]).
node_id_label(subtree(Split, Tree), Id, Label) :-
    term_hash(subtree(Split, Tree), Hash),
    format(atom(Id), '"~k_~d"', [Split, Hash]),
    format(atom(Label), '"subtree(~k)"', [Split]).
node_id_label(node(Left, Split, Right), Id, Label) :-
    term_hash(node(Left, Split, Right), Hash),
    format(atom(Id), '"~k_~d"', [Split, Hash]),
    format(atom(Label), '"~k?"', [Split]).


print_dottree(Fmt) :-
    catch(
        once(fmt_tree_maxbits(Fmt, Tree, _)),
        error(_, _),
        Tree = empty
    ),
    dotprint_tree(Fmt, Tree).


print_dottrees :-
    call_time(
        foreach(
            (
                isa:fmt(Fmt),
                Fmt \= ext,
                format(atom(Path), 'assets/graphs/~k.dot', [Fmt])
            ),
            utils:output_to_file(Path, optree:print_dottree(Fmt))
        ),
        Time
    ),
    format('[`print_dottrees` ran in ~3fs]~n', Time.wall).

