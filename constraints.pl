employee_max_shifts(joy, 12).
employee_max_shifts(pernilla, 12).
employee_max_shifts(gun, 12).
employee_max_shifts(annmarie, 12).

employee_skills(rebecca, [gastro, mmt]).
employee_skills(josefin, [imed, akut]).
employee_skills(emelie, [ultraljud, hud]).

task_skills(outpatient_ward, []).
task_skills(neutering, [dogs, cats]).
task_skills(horseshoe_maintenance, [horses]).
task_skills(pharmacy, [medicines]).

employee_unavailable(kava, shift(thursday_3, _)).

task(gastro, shift(tuesday, daytime)).
task(gastro, shift(thursday, daytime)).
task(hud, shift(monday, daytime)).


employee_assigned(kava, task(hud, shift(monday, daytime))).
employee_assigned(kava, task(hud, shift(wednesday, evening))).
employee_assigned(kava, task(hud, shift(friday_3, daytime))).
