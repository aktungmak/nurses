:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

:- dynamic employee_max_shifts/2.
:- dynamic employee_skills/2.
:- dynamic task_skills/2.
:- dynamic employee_unavailable/2.
:- dynamic task/2.
:- dynamic employee_assigned/2.

% Load a file containing constraint definintions and print a suitable schedule
run(ConstraintsFile) :-
    consult(ConstraintsFile),
    schedule(Schedule),
    print_term(Schedule, []).

% Generate a schedule mapping employees to tasks, consistent with the constraints
schedule(Schedule) :-
    tasks(Ts),
    employees(Es),
    tasks_employees_assoc(Ts, Es, Assoc),
    assoc_to_keys(Assoc, AssocKeys),
    assoc_to_values(Assoc, AssocValues),
    constraints(Ts, Es, Assoc),
    label(AssocValues),
    findall(AssocKey,
            (member(AssocKey, AssocKeys), get_assoc(AssocKey, Assoc, 1)),
            Schedule).

% Conjuction of all required constraints on a schedule. New constraints can be
% appended to this rule.
constraints(Ts, Es, Assoc) :-
    one_employee_per_task(Ts, Es, Assoc),
    print("one_employee_per_task: ok!"),
    no_multitasking(Ts, Es, Assoc),
    print(no_multitasking),
    max_shifts_constraints(Assoc,Es,Ts),
    print(main),
    unavailable_constraints(Assoc,Es,Ts),
    print(1),
    skills_constraints(Assoc,Es,Ts),
    print(1),
    assigned_constraints(Assoc).

one_employee_per_task(Ts, Es, Assoc) :-
    maplist(one_employee_per_task_(Assoc, Es), Ts).

one_employee_per_task_(Assoc, Es, T) :-
    findall(assign(T, E), member(E, Es), Keys),
    assoc_keys_vars(Assoc, Keys, Vars),
    list_or(Vars, Assignments),
    Assignments.

no_multitasking(Ts, Es, Assoc) :-
    shifts(Shifts),
    findall(employee_shift(E,Shift), (member(E,Es),member(Shift,Shifts)), EmployeeShifts),
    maplist(no_multitasking_(Assoc,Ts),EmployeeShifts).

no_multitasking_(Assoc,Ts,employee_shift(E,Shift)) :-
    findall(task(TName,Shift),member(task(TName,Shift),Ts),ShiftTs),
    findall(assign(T,E),member(T,ShiftTs),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=<,1).

max_shifts_constraints(Assoc,Es,Ts) :-
    maplist(max_shifts_subexpr(Assoc,Ts),Es).

max_shifts_subexpr(Assoc,Ts,E) :-
    E = employee(EName),
    employee_max_shifts(EName,MaxShifts),
    findall(assign(T,E),member(T,Ts),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=<,MaxShifts).

unavailable_constraints(Assoc,Es,Ts) :-
    findall(assign(T,E),(
            member(E,Es),
            E = employee(EName),
            employee_unavailable(EName,Shift),
            member(T,Ts),
            T = task(_TName,Shift)
        ),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    maplist(#=(0),Vars).

skills_constraints(Assoc,Es,Ts) :-
    findall(assign(T,E),(
            member(T,Ts),
            T = task(TName,_TShift),
            task_skills(TName,TSkills),
            member(E,Es),
            \+employee_has_skills(E,TSkills)
        ),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    maplist(#=(0),Vars).

employee_has_skills(employee(EName),Skills) :-
    employee_skills(EName, ESkills),
    subset(Skills, ESkills).

assigned_constraints(Assoc) :-
    findall(assign(T,E),(
            employee_assigned(EName,T),
            E = employee(EName)
        ),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    maplist(#=(1),Vars).

employees(Employees) :-
    findall(employee(E), employee_skills(E, _), Employees).

tasks(Tasks) :-
    findall(task(TName,TShift), task(TName,TShift), Tasks).

shifts(Shifts) :-
    setof(Shift, TName^task(TName, Shift), Shifts).

tasks_employees_assoc(Ts, Es, Assoc) :-
    findall(assign(T, E), (member(T, Ts), member(E, Es)), Assignments),
    empty_assoc(EmptyAssoc),
    foldl(put_assoc_, Assignments, EmptyAssoc, Assoc).

put_assoc_(K, InAssoc, OutAssoc) :-
    put_assoc(K, InAssoc, _, OutAssoc).

assoc_keys_vars(Assoc, Keys, Vars) :-
        maplist(assoc_key_var(Assoc), Keys, Vars).
assoc_key_var(Assoc, Key, Var) :- get_assoc(Key, Assoc, Var).

list_or([L|Ls], Or) :- foldl(disjunction_, Ls, L, Or).
disjunction_(A, B, B#\/A).
