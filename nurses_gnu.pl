
% Load a file containing constraint definintions and print a suitable schedule
run(ConstraintsFile) :-
    consult(ConstraintsFile),
    schedule(Schedule),
    print_term(Schedule, []).

% Generate a schedule mapping employees to tasks, consistent with constraints/3
schedule(Schedule) :-
    employees(Es),
    tasks(Ts),
    tasks_employees_pairs(Ts, Es, Pairs),
    constraints(Pairs),
    pairs_values(Pairs, Values),
    label(Values),
    findall(Keys, pairs_keys_value(Pairs, Keys, 1), Schedule).

% Conjuction of all required constraints on a schedule. New constraints can be
% appended to this rule.
constraints(Pairs) :-
    one_employee_per_task(Pairs), print(one_employee_per_task_ok), nl,
    no_multitasking(Pairs), print(no_multitasking_ok), nl,
    max_shifts_constraints(Pairs), print(max_shifts_constraints_ok), nl,
    unavailable_constraints(Pairs), print(unavailable_constraints_ok), nl,
    skills_constraints(Pairs), print(skills_constraints_ok), nl,
    assigned_constraints(Pairs), print(assigned_constraints_ok), nl.

one_employee_per_task(Assignments) :-
    group_pairs_by_key(Assignments, GroupedAssignments),
    pairs_values(GroupedAssignments, Values),
    maplist(fd_only_one, Values).

no_multitasking(Ts, Es, Assoc) :-
    shifts(Shifts),
    findall(employee_shift(E,Shift), (member(E,Es),member(Shift,Shifts)), EmployeeShifts),
    maplist(no_multitasking_(Assoc,Ts),EmployeeShifts).

no_multitasking_(Assoc,Ts,employee_shift(E,Shift)) :-
    findall(task(TName,Shift),member(task(TName,Shift),Ts),ShiftTs),
    findall(assign(T,E),member(T,ShiftTs),Keys),
    pairs_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=<,1).

max_shifts_constraints(Assoc,Es,Ts) :-
    maplist(max_shifts_subexpr(Assoc,Ts),Es).

max_shifts_subexpr(Assoc,Ts,E) :-
    E = employee(EName),
    employee_max_shifts(EName,MaxShifts),
    findall(assign(T,E),member(T,Ts),Keys),
    pairs_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=<,MaxShifts).

unavailable_constraints(Assoc,Es,Ts) :-
    findall(assign(T,E),(
            member(E,Es),
            E = employee(EName),
            employee_unavailable(EName,Shift),
            member(T,Ts),
            T = task(_TName,Shift)
        ),Keys),
    pairs_keys_vars(Assoc,Keys,Vars),
    maplist(#=(0),Vars).

skills_constraints(Assoc,Es,Ts) :-
    findall(assign(T,E),(
            member(T,Ts),
            T = task(TName,_TShift),
            task_skills(TName,TSkills),
            member(E,Es),
            \+employee_has_skills(E,TSkills)
        ),Keys),
    pairs_keys_vars(Assoc,Keys,Vars),
    maplist(#=(0),Vars).

employee_has_skills(employee(EName), Skills) :-
    employee_skills(EName, ESkills),
    subset(Skills, ESkills).

assigned_constraints(Assoc) :-
    findall(assign(T,E),(
            employee_assigned(EName,T),
            E = employee(EName)
        ),Keys),
    pairs_keys_vars(Assoc,Keys,Vars),
    maplist(#=(1),Vars).

employees(Employees) :-
    findall(employee(E), employee_skills(E, _), Employees).

tasks(Tasks) :-
    findall(task(TName,TShift), task(TName,TShift), Tasks).

shifts(Shifts) :-
    setof(Shift, TName^task(TName, Shift), Shifts).

tasks_employees_pairs(Ts, Es, Pairs) :-
    findall(assign(T, E)-_, (member(T, Ts), member(E, Es)), Pairs).

pairs_keys_values(Pairs, Keys, Values) :-
    maplist(pair_key_var(Pairs), Keys, Values).
pairs_keys(Pairs, Keys) :-
    maplist(pair_key(Pairs), Keys).
pairs_values(Pairs, Values) :-
    maplist(pair_value(Pairs), Values).

pair_key_value(K-V, K, V).
pair_key(K-_, K).
pair_value(_-V, V).

pairs_keys_value([], _, _).
pairs_keys_value([K-V|Ps], [K|Ks], V) :-
  pairs_keys_value(Ps, Ks, V).
pairs_keys_value([_-V1|Ps], Ks, V) :-
  V1 \== V,
  pairs_keys_value(Ps, Ks, V).

% taken from SWI-prolog's pairs.pl
group_pairs_by_key([], []).
group_pairs_by_key([M-N|T0], [M-[N|TN]|T]) :-
    same_key(M, T0, TN, T1),
    group_pairs_by_key(T1, T).
same_key(M0, [M-N|T0], [N|TN], T) :-
    M0 == M,
    !,
    same_key(M, T0, TN, T).
same_key(_, L, [], L).
