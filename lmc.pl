%%%% -*- Mode : Prolog -*-


%%% VALIDAZIONI SULLO STATE

%%% Funzioni accessorie a is-valid-state/1

is_valid_mem_value(X) :-
    integer(X),
    between(0, 999, X).

%%% Un indice valido di cella di memoria e un valore valido per il pc
is_valid_cellmem(X) :-
    integer(X),
    between(0, 99, X).

acc_has_valid_value(X) :-
    is_valid_mem_value(X).

pc_has_valid_value(X) :-
    is_valid_cellmem(X).

list_has_valid_values([]).

list_has_valid_values([H | T]) :-
    is_valid_mem_value(H),
    list_has_valid_values(T).

mem_is_valid(Mem) :-
    proper_length(Mem, 100),
    list_has_valid_values(Mem).

in_is_valid(In) :-
    is_list(In),
    list_has_valid_values(In).

is_valid_flag(Flag) :-
    atom(Flag),
    Flag == flag,
    !.

is_valid_flag(Flag) :-
    atom(Flag),
    Flag == noflag,
    !.

%%% IS-VALID-STATE/1
%%% Usata in one-instruction per assicurarsi che lo stato passato come 
%%% argomento della funzione sia valido
is_valid_state(state(Acc, Pc, Mem, In, Out, Flag)) :-
    acc_has_valid_value(Acc),
    pc_has_valid_value(Pc),
    proper_length(Mem, 100),
    is_list(In),
    is_list(Out),
    is_valid_flag(Flag).

is_valid_state(halted_state(Acc, Pc, Mem, In, Out, Flag)) :-
    acc_has_valid_value(Acc),
    pc_has_valid_value(Pc),
    proper_length(Mem, 100),
    is_list(In),
    is_list(Out),
    is_valid_flag(Flag).


%%% STATI

state(Acc, Pc, Mem, In, Out, Flag) :- 
    is_valid_state(state(Acc, Pc, Mem, In, Out, Flag)).

halted_state(Acc, Pc, Mem, In, Out, Flag) :- 
    is_valid_state(halted_state(Acc, Pc, Mem, In, Out, Flag)).


%%% ISTRUZIONI LMC

%%% REPLACE_VALUE/4
%%% Predicato per sostituire il valore nella memoria.
replace_value(Counter, [_ | Ts], Value, [Value | Ts]) :-
    Counter == 0,
    !.

replace_value(Counter, [H | Ts], Value, [H | Rest]) :-
    !,
    NewCounter is Counter - 1,
    replace_value(NewCounter, Ts, Value, Rest).

%%% ONE-INSTRUCTION/2
%%% Esecuzione di una singola istruzione
%%% Restituisce il nuovo stato derivato dall'esecuzione dell'istruzione
%%% Fallisce se era già stato eseguita un'istruzione di halt,
%%% se l'istruzione è di input ma la lista di input è vuota, oppure se
%%% l'istruzione da eseguire non è valida

%% HLT
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, _),
    Op =:= 0,
    !,
    NewState =.. [halted_state, Acc, Pc, Mem, In, Out, Flag].

%% ADD (RISULTATO < 1000)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 1,
    nth0(CellMem, Mem, Value),
    is_valid_mem_value(Value),
    Acc + Value < 1000,
    !,
    Sum is (Acc + Value) mod 1000,
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Sum, IncrementedPc, Mem, In, Out, noflag].

%% ADD (RISULTATO >= 1000)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 1,
    nth0(CellMem, Mem, Value),
    is_valid_mem_value(Value),
    Acc + Value >= 1000,
    !,
    Sum is (Acc + Value) mod 1000,
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Sum, IncrementedPc, Mem, In, Out, flag].

%% SUB (RISULTATO < 0)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 2,
    nth0(CellMem, Mem, Value),
    is_valid_mem_value(Value),
    Acc - Value < 0,
    !,
    Sub is (Acc - Value) mod 1000,
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Sub, IncrementedPc, Mem, In, Out, flag].

%% SUB (RISULTATO >= 0)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 2,
    nth0(CellMem, Mem, Value),
    is_valid_mem_value(Value),
    Acc - Value >= 0,
    !,
    Sub is (Acc - Value) mod 1000,
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Sub, IncrementedPc, Mem, In, Out, noflag].

%% STA
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 3,
    !,
    replace_value(CellMem, Mem, Acc, NewMem),
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Acc, IncrementedPc, NewMem, In, Out, Flag].

%% LDA
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 5,
    !,
    nth0(CellMem, Mem, Value),
    is_valid_mem_value(Value),
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Value, IncrementedPc, Mem, In, Out, Flag].

%% BRA
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 6,
    !,
    IncrementedPc is CellMem,
    NewState =.. [state, Acc, IncrementedPc, Mem, In, Out, Flag].

%% BRZ (ACC == 0)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 7,
    Acc =:= 0,
    Flag == noflag,
    !,
    IncrementedPc is CellMem,
    NewState =.. [state, Acc, IncrementedPc, Mem, In, Out, Flag].

%% BRZ (ACC =\= 0)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, _),
    Op =:= 7,
    !,
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Acc, IncrementedPc, Mem, In, Out, Flag].

%% BRP (Flag == noflag)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, CellMem),
    Op =:= 8,
    Flag == noflag,
    !,
    IncrementedPc is CellMem,
    NewState =.. [state, Acc, IncrementedPc, Mem, In, Out, Flag].

%% BRP (Flag == flag)
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    divmod(MemValue, 100, Op, _),
    Op =:= 8,
    Flag == flag,
    !,
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Acc, IncrementedPc, Mem, In, Out, Flag].

%% INP
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    MemValue =:= 901,
    !,
    nth0(0, In, Input, NewIn),
    is_valid_mem_value(Input),
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Input, IncrementedPc, Mem, NewIn, Out, Flag].

%% OUT
one_instruction(State, NewState) :-
    State =.. [state, Acc, Pc, Mem, In, Out, Flag],
    state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, MemValue),
    MemValue =:= 902,
    !,
    append(Out, [Acc], NewOut),
    IncrementedPc is (Pc + 1) mod 100,
    NewState =.. [state, Acc, IncrementedPc, Mem, In, NewOut, Flag].


%%% ESECUZIONE ISTRUZIONI IN MEMORIA

%%% EXECUTION-LOOP/2
%%% Esecuzione delle istruzioni in memoria.
%%% Produce il contenuto della coda di output dopo l’arresto del LMC.
%%% Fallisce se l’esecuzione termina senza eseguire una istruzione di halt.

execution_loop(State, Out) :-
    functor(State, state, 6),
    !,
    one_instruction(State, NewState),
    execution_loop(NewState, Out).

execution_loop(State, Out) :-
    functor(State, halted_state, 6),
    !,
    State =.. [halted_state, _, _, _, _, Out, _].


%%% PARSING ISTRUZIONI ASSEMBLY

%%% Decodifica codice assembly
%%% Divisione degli elementi in istruzioni, commenti, valori, etichette

is_label(Word) :-
    \+is_comment(Word),
    \+is_value(Word, _),
    \+is_instruction(Word, _),
    string(Word),
    string_length(Word, N),
    N > 0,
    string_chars(Word, [Char | _]),
    char_type(Char, alpha).

is_comment(Word) :-
    string(Word),
    string_chars(Word, [FirstChar, SecondChar | _]),
    FirstChar == '/',
    SecondChar == '/'.

%%% IS-VALUE/2
%%% Se il valore è valido, lo restituisce convertendolo da stringa a intero
is_value(Word, Value) :-
    string(Word),
    number_string(Value, Word),
    is_valid_mem_value(Value).

%%% IS-INSTRUCTION/2
%%% Se l'istruzione è valida, ne ritorna l'opcode
is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "ADD",
    !,
    OpCode is 100.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "SUB",
    !,
    OpCode is 200.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "STA",
    !,
    OpCode is 300.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "LDA",
    !,
    OpCode is 500.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "BRA",
    !,
    OpCode is 600.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "BRZ",
    !,
    OpCode is 700.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "BRP",
    !,
    OpCode is 800.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "INP",
    !,
    OpCode is 901.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "OUT",
    !,
    OpCode is 902.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "HLT",
    !,
    OpCode is 0.

is_instruction(Word, OpCode) :-
    string(Word),
    string_upper(Word, Instr),
    Instr == "DAT",
    !,
    OpCode is 0.


%%% ISTRUZIONI ASSEMBLY VALIDE

%%% READ-WORDS/3
%%% Riconosce le righe di codice assembly valide e restituisce 
%%% l'istruzione corrispondente
read_words([], _, _) :- !.

%% ISTRUZIONE CELLMEM
read_words([First, Second | Rest], Instr, _) :-
    is_instruction(First, OpCode),
    between(100, 899, OpCode),
    is_value(Second, Value),
    is_valid_cellmem(Value),
    Rest == [],
    !,
    Instr is (OpCode + Value).

%% ISTRUZIONE CELLMEM (DAT XXX)
read_words([First, Second | Rest], Instr, _) :-
    is_instruction(First, OpCode),
    OpCode =:= 0,
    is_value(Second, Value),
    Rest == [],
    !,
    Instr is (OpCode + Value).

%% ISTRUZIONE (INP / OUT / HLT / DAT)
read_words([First | Rest], Instr, _) :-
    is_instruction(First, OpCode),
    Rest == [],
    !,
    Instr is OpCode.

%% LABEL ISTRUZIONE CELLMEM
read_words([First, Second, Third | Rest], Instr, _) :-
    is_label(First),
    is_instruction(Second, OpCode),
    between(100, 899, OpCode),
    is_value(Third, Value),
    is_valid_cellmem(Value),
    Rest == [],
    !,
    Instr is (OpCode + Value).

%% LABEL ISTRUZIONE CELLMEM (DAT XXX)
read_words([First, Second, Third | Rest], Instr, _) :-
    is_label(First),
    is_instruction(Second, OpCode),
    OpCode =:= 0,
    is_value(Third, Value),
    Rest == [],
    !,
    Instr is (OpCode + Value).

%% LABEL ISTRUZIONE (INP / OUT / HLT / DAT)
read_words([First, Second | Rest], Instr, _) :-
    is_label(First),
    is_instruction(Second, OpCode),
    Rest == [],
    !,
    Instr is OpCode.

%% ISTRUZIONE LABEL
read_words([First, Second | Rest], Instr, Labels) :-
    is_instruction(First, OpCode),
    between(100, 899, OpCode),
    is_label(Second),
    Rest == [],
    !,
    string_upper(Second, SecondUp),
    nextto(SecondUp, Value, Labels),
    Instr is (OpCode + Value).

%% LABEL ISTRUZIONE LABEL
read_words([First, Second, Third | Rest], Instr, Labels) :-
    is_label(First),
    is_instruction(Second, OpCode),
    between(100, 899, OpCode),
    is_label(Third),
    Rest == [],
    !,
    string_upper(Third, ThirdUp),
    nextto(ThirdUp, Value, Labels),
    Instr is (OpCode + Value).


%%% CARICAMENTO IN MEMORIA DELLE ISTRUZIONI DI UN FILE

%%% REMOVE_COMMENT/2
%%% Riconoscimento e rimozione commenti dalle istruzioni assembly.
remove_comment([], []) :- !.

remove_comment([Word | _], NewRest) :-
    is_comment(Word),
    NewRest = [],
    !.

remove_comment([Word | Rest], [Word | NewRest]) :-
    remove_comment(Rest, NewRest).


%%% SAVE_LABELS/3
%%% Salvataggio delle etichette dichiarate nel file assembly e 
%%% associazione con il valore 

save_labels([], [], _) :- !.

%%% RIGA VUOTA
save_labels([Line | Lines], Rest, NumInstr) :-
    split_string(Line, "\s\t", "\s\t", Words),
    remove_comment(Words, NewWords),
    NewWords == [""],
    !,
    save_labels(Lines, Rest, NumInstr).

%%% SOLO COMMENTO
save_labels([Line | Lines], Rest, NumInstr) :-
    split_string(Line, "\s\t\v", "\s\t\v", Words),
    remove_comment(Words, NewWords),
    NewWords == [],
    !,
    save_labels(Lines, Rest, NumInstr).

%%% DICHIARAZIONE LABEL
save_labels([Line | Lines], [LabelUp, NumInstr | Rest], NumInstr) :-
    NumInstr < 100,
    split_string(Line, "\s\t\v", "\s\t\v", Words),
    remove_comment(Words, NewWords),
    nth0(0, NewWords, Label),
    is_label(Label),
    !,
    string_upper(Label, LabelUp),
    NextInstr is NumInstr + 1, 
    save_labels(Lines, Rest, NextInstr),
    \+memberchk(LabelUp, Rest).

%%% NO DICHIARAZIONE LABEL
save_labels([_ | Lines], Rest, NumInstr) :-
    NumInstr < 100,
    !,
    NextInstr is NumInstr + 1, 
    save_labels(Lines, Rest, NextInstr).


%%% READ_LINES/3
%%% Rimuove ricorsivamente commenti dal codice assembly 
%%% riga per riga e ignora righe vuote.

read_lines([], [], _) :- !.

%%% COMMENTO
read_lines([First | Rest], Mem, Labels) :-
    split_string(First, "\s\t\v", "\s\t\v", Words),
    remove_comment(Words, NewWords),
    NewWords == [],
    !,
    read_lines(Rest, Mem, Labels).

%%% RIGA VUOTA
read_lines([First | Rest], Mem, Labels) :-
    split_string(First, "\s\t\v", "\s\t\v", Words),
    remove_comment(Words, NewWords),
    NewWords == [""],
    !,
    read_lines(Rest, Mem, Labels).

read_lines([First | Rest], [Instr | Mem], Labels) :-
    split_string(First, "\s\t\v", "\s\t\v", Words),
    remove_comment(Words, NewWords),
    read_words(NewWords, Instr, Labels),
    !,
    read_lines(Rest, Mem, Labels).

%%% READ_LINES/2
%%% Predicato di passaggio per il salvataggio delle etichette dichiarate
%%% nel codice assembly. 
read_lines(Lines, Mem) :-
    save_labels(Lines, Labels, 0),
    read_lines(Lines, Mem, Labels).

%%% FILL_MEMORY/2
%%% Controlla che la memoria sia costituita da 100 elementi.
%%% Nel caso non lo sia, finisce di riempirla.

%%% MEMORIA PIENA
fill_memory(Instrs, Mem) :-
    length(Instrs, Length),
    Length =:= 100,
    !,
    Mem = Instrs.

%%% MEMORIA VUOTA
fill_memory(Instrs, Mem) :-
    length(Instrs, Length),
    Length =:= 0,
    !,
    randseq(99, 99, RestOfMem),
    append([0], RestOfMem, Mem).

fill_memory(Instrs, Mem) :-
    length(Instrs, Length),
    Length < 100,
    !,
    randseq((100 - Length), 99, RestOfMem),
    append(Instrs, RestOfMem, Mem).

%%% LMC_LOAD/2
%%% Legge un file che contiene un codice assembly e che produce il 
%%% contenuto iniziale della memoria del sistema
lmc_load(Filename, Mem) :-
    read_file_to_codes(Filename, File, []),
    split_string(File, "\r\n\f", "\r\n\f", Lines),
    read_lines(Lines, Instrs),
    fill_memory(Instrs, Mem).


%%% ESECUZIONE DI UN PROGRAMMA ASSEMBLY
%%% LMC-RUN/3
%%% Legge un file che contiene codice assembly, lo carica con lmc-load e
%%% inizializza la coda di input al valore fornito.
%%% Produce un output che è il risultato dell’invocazione di execution-loop.
lmc_run(Filename, In, Out) :-
    in_is_valid(In),
    lmc_load(Filename, Mem),
    mem_is_valid(Mem),
    State =.. [state, 0, 0, Mem, In, [], noflag],
    execution_loop(State, Out).
