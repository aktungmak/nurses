employee_max_shifts(joy, 12).
employee_max_shifts(pernilla, 12).
employee_max_shifts(gun, 12).
employee_max_shifts(annmarie, 12).

employee_skills(joy, [dogs, cats]).
employee_skills(pernilla, [dogs, cats, horses]).
employee_skills(gun, [birds]).
employee_skills(annmarie, [medicines]).

task_skills(triage, [dogs, cats]).
task_skills(neutering, [dogs, cats]).
task_skills(horseshoe_maintenance, [horses]).
task_skills(pharmacy, [medicines]).

employee_unavailable(joy, shift(friday, 1)).
employee_unavailable(joy, shift(friday, 2)).
employee_unavailable(joy, shift(saturday, 1)).
employee_unavailable(joy, shift(saturday, 2)).
employee_unavailable(pernilla, shift(sunday, _)).

task(neutering, shift(saturday, 1)).
task(neutering, shift(monday, 2)).
task(triage, shift(monday, 1)).
task(triage, shift(monday, 2)).
task(triage, shift(tuesday, 1)).
task(triage, shift(tuesday, 2)).
task(triage, shift(wednesday, 1)).
task(triage, shift(wednesday, 2)).
task(pharmacy, shift(monday, 1)).
task(pharmacy, shift(monday, 2)).
task(pharmacy, shift(tuesday, 1)).
task(pharmacy, shift(tuesday, 2)).
task(pharmacy, shift(wednesday, 1)).
task(pharmacy, shift(wednesday, 2)).
task(horseshoe_maintenance, shift(friday, 1)).

employee_assigned(joy, task(triage, shift(monday, 1))).
employee_assigned(pernilla, task(triage, shift(monday, 2))).
employee_assigned(joy, task(triage, shift(tuesday, 1))).
employee_assigned(joy, task(triage, shift(tuesday, 2))).
