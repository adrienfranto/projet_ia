% Consigne projet IA MI G5 IG :
% Projet basé sur la programmation logique (PROLOG)
% Vous allez faire un programme PROLOG sur l'enquête policière (ou judiciaire)
% à rendre sur : josue.ratovondrahona@gmail.com (de préférence code sur gitlab ou github)
% Deadline 24 Août 2025

% Types de crime
crime_type(assassinat).

% Faits
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).

has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).

has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% Règles
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    has_fingerprint_on_weapon(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    (has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    (has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ).

% Entrée principale
main :-
    current_input(Input),
    read(Input, crime(Suspect, CrimeType)),
    (is_guilty(Suspect, CrimeType) ->
        writeln(guilty)
    ; writeln(not_guilty)
    ),
    halt.

% ==========================================
% 100 REQUÊTES PROLOG POUR L'ENQUÊTE POLICIÈRE
% ==========================================

% === REQUÊTES 1-20: VÉRIFICATION DE CULPABILITÉ ===
% 1. ?- is_guilty(john, vol).
% 2. ?- is_guilty(mary, assassinat).
% 3. ?- is_guilty(alice, escroquerie).
% 4. ?- is_guilty(bruno, escroquerie).
% 5. ?- is_guilty(sophie, escroquerie).
% 6. ?- is_guilty(john, assassinat).
% 7. ?- is_guilty(mary, vol).
% 8. ?- is_guilty(alice, vol).
% 9. ?- is_guilty(bruno, vol).
% 10. ?- is_guilty(sophie, vol).
% 11. ?- is_guilty(john, escroquerie).
% 12. ?- is_guilty(mary, escroquerie).
% 13. ?- is_guilty(alice, assassinat).
% 14. ?- is_guilty(bruno, assassinat).
% 15. ?- is_guilty(sophie, assassinat).
% 16. ?- is_guilty(X, vol).
% 17. ?- is_guilty(X, assassinat).
% 18. ?- is_guilty(X, escroquerie).
% 19. ?- is_guilty(john, X).
% 20. ?- is_guilty(X, Y).

% === REQUÊTES 21-40: VÉRIFICATION DES FAITS ===
% 21. ?- suspect(john).
% 22. ?- suspect(mary).
% 23. ?- suspect(alice).
% 24. ?- suspect(bruno).
% 25. ?- suspect(sophie).
% 26. ?- suspect(X).
% 27. ?- crime_type(assassinat).
% 28. ?- crime_type(X).
% 29. ?- has_motive(john, vol).
% 30. ?- has_motive(mary, assassinat).
% 31. ?- has_motive(alice, escroquerie).
% 32. ?- has_motive(X, vol).
% 33. ?- has_motive(X, assassinat).
% 34. ?- has_motive(X, escroquerie).
% 35. ?- has_motive(john, X).
% 36. ?- has_motive(mary, X).
% 37. ?- has_motive(alice, X).
% 38. ?- has_motive(X, Y).
% 39. ?- was_near_crime_scene(john, vol).
% 40. ?- was_near_crime_scene(mary, assassinat).

% === REQUÊTES 41-60: EMPREINTES ET PREUVES ===
% 41. ?- has_fingerprint_on_weapon(john, vol).
% 42. ?- has_fingerprint_on_weapon(mary, assassinat).
% 43. ?- has_fingerprint_on_weapon(X, vol).
% 44. ?- has_fingerprint_on_weapon(X, assassinat).
% 45. ?- has_fingerprint_on_weapon(john, X).
% 46. ?- has_fingerprint_on_weapon(mary, X).
% 47. ?- has_fingerprint_on_weapon(X, Y).
% 48. ?- has_bank_transaction(alice, escroquerie).
% 49. ?- has_bank_transaction(bruno, escroquerie).
% 50. ?- has_bank_transaction(X, escroquerie).
% 51. ?- has_bank_transaction(alice, X).
% 52. ?- has_bank_transaction(bruno, X).
% 53. ?- has_bank_transaction(X, Y).
% 54. ?- owns_fake_identity(sophie, escroquerie).
% 55. ?- owns_fake_identity(X, escroquerie).
% 56. ?- owns_fake_identity(sophie, X).
% 57. ?- owns_fake_identity(X, Y).
% 58. ?- was_near_crime_scene(X, vol).
% 59. ?- was_near_crime_scene(X, assassinat).
% 60. ?- was_near_crime_scene(X, Y).

% === REQUÊTES 61-80: REQUÊTES COMBINÉES ET LOGIQUES ===
% 61. ?- has_motive(X, vol), was_near_crime_scene(X, vol).
% 62. ?- has_motive(X, assassinat), was_near_crime_scene(X, assassinat).
% 63. ?- has_motive(X, escroquerie), has_bank_transaction(X, escroquerie).
% 64. ?- has_motive(X, escroquerie), owns_fake_identity(X, escroquerie).
% 65. ?- suspect(X), has_motive(X, vol).
% 66. ?- suspect(X), has_motive(X, assassinat).
% 67. ?- suspect(X), has_motive(X, escroquerie).
% 68. ?- suspect(X), is_guilty(X, vol).
% 69. ?- suspect(X), is_guilty(X, assassinat).
% 70. ?- suspect(X), is_guilty(X, escroquerie).
% 71. ?- suspect(X), is_guilty(X, Y).
% 72. ?- suspect(X), \+ is_guilty(X, vol).
% 73. ?- suspect(X), \+ is_guilty(X, assassinat).
% 74. ?- suspect(X), \+ is_guilty(X, escroquerie).
% 75. ?- suspect(X), \+ is_guilty(X, _).
% 76. ?- has_motive(X, Y), \+ is_guilty(X, Y).
% 77. ?- was_near_crime_scene(X, Y), \+ is_guilty(X, Y).
% 78. ?- has_fingerprint_on_weapon(X, Y), is_guilty(X, Y).
% 79. ?- has_bank_transaction(X, Y), is_guilty(X, Y).
% 80. ?- owns_fake_identity(X, Y), is_guilty(X, Y).

% === REQUÊTES 81-100: REQUÊTES AVANCÉES ET ANALYSES ===
% 81. ?- findall(X, suspect(X), Suspects).
% 82. ?- findall(X, crime_type(X), Crimes).
% 83. ?- findall(X, is_guilty(X, vol), VolCoupables).
% 84. ?- findall(X, is_guilty(X, assassinat), AssassinatCoupables).
% 85. ?- findall(X, is_guilty(X, escroquerie), EscroquerieCoupables).
% 86. ?- findall(X-Y, is_guilty(X, Y), TousCoupables).
% 87. ?- findall(X, has_motive(X, vol), MotifsVol).
% 88. ?- findall(X, has_motive(X, assassinat), MotifsAssassinat).
% 89. ?- findall(X, has_motive(X, escroquerie), MotifsEscroquerie).
% 90. ?- findall(X-Y, has_motive(X, Y), TousMotifs).
% 91. ?- forall(suspect(X), (write(X), write(' est suspect'), nl)).
% 92. ?- forall(is_guilty(X, Y), (write(X), write(' coupable de '), write(Y), nl)).
% 93. ?- bagof(X, is_guilty(X, vol), VolCoupables).
% 94. ?- bagof(X, is_guilty(X, assassinat), AssassinatCoupables).
% 95. ?- bagof(X, is_guilty(X, escroquerie), EscroquerieCoupables).
% 96. ?- setof(X, is_guilty(X, _), TousCoupablesUniques).
% 97. ?- setof(Y, is_guilty(_, Y), CrimesAvecCoupables).
% 98. ?- aggregate_all(count, is_guilty(_, vol), NombreVolCoupables).
% 99. ?- aggregate_all(count, is_guilty(_, _), NombreTotalCoupables).
% 100. ?- once(is_guilty(X, Y)).

% === LANCEMENT AUTOMATIQUE DES 100 REQUÊTES AVEC RÉPONSES ===
run_all_100_queries :-
    write('=========================================='), nl,
    write('EXÉCUTION AUTOMATIQUE DES 100 REQUÊTES'), nl,
    write('=========================================='), nl, nl,

    % REQUÊTES 1-20: VÉRIFICATION DE CULPABILITÉ
    write('=== REQUÊTES 1-20: VÉRIFICATION DE CULPABILITÉ ==='), nl,
    write('1. is_guilty(john, vol): '), (is_guilty(john, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('2. is_guilty(mary, assassinat): '), (is_guilty(mary, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('3. is_guilty(alice, escroquerie): '), (is_guilty(alice, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('4. is_guilty(bruno, escroquerie): '), (is_guilty(bruno, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('5. is_guilty(sophie, escroquerie): '), (is_guilty(sophie, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('6. is_guilty(john, assassinat): '), (is_guilty(john, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('7. is_guilty(mary, vol): '), (is_guilty(mary, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('8. is_guilty(alice, vol): '), (is_guilty(alice, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('9. is_guilty(bruno, vol): '), (is_guilty(bruno, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('10. is_guilty(sophie, vol): '), (is_guilty(sophie, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('11. is_guilty(john, escroquerie): '), (is_guilty(john, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('12. is_guilty(mary, escroquerie): '), (is_guilty(mary, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('13. is_guilty(alice, assassinat): '), (is_guilty(alice, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('14. is_guilty(bruno, assassinat): '), (is_guilty(bruno, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('15. is_guilty(sophie, assassinat): '), (is_guilty(sophie, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('16. is_guilty(X, vol): '), findall(X, is_guilty(X, vol), R16), write(R16), nl,
    write('17. is_guilty(X, assassinat): '), findall(X, is_guilty(X, assassinat), R17), write(R17), nl,
    write('18. is_guilty(X, escroquerie): '), findall(X, is_guilty(X, escroquerie), R18), write(R18), nl,
    write('19. is_guilty(john, X): '), findall(X, is_guilty(john, X), R19), write(R19), nl,
    write('20. is_guilty(X, Y): '), findall(X-Y, is_guilty(X, Y), R20), write(R20), nl,
    nl,

    % REQUÊTES 21-40: VÉRIFICATION DES FAITS
    write('=== REQUÊTES 21-40: VÉRIFICATION DES FAITS ==='), nl,
    write('21. suspect(john): '), (suspect(john) -> write('TRUE') ; write('FALSE')), nl,
    write('22. suspect(mary): '), (suspect(mary) -> write('TRUE') ; write('FALSE')), nl,
    write('23. suspect(alice): '), (suspect(alice) -> write('TRUE') ; write('FALSE')), nl,
    write('24. suspect(bruno): '), (suspect(bruno) -> write('TRUE') ; write('FALSE')), nl,
    write('25. suspect(sophie): '), (suspect(sophie) -> write('TRUE') ; write('FALSE')), nl,
    write('26. suspect(X): '), findall(X, suspect(X), R26), write(R26), nl,
    write('27. crime_type(assassinat): '), (crime_type(assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('28. crime_type(X): '), findall(X, crime_type(X), R28), write(R28), nl,
    write('29. has_motive(john, vol): '), (has_motive(john, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('30. has_motive(mary, assassinat): '), (has_motive(mary, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('31. has_motive(alice, escroquerie): '), (has_motive(alice, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('32. has_motive(X, vol): '), findall(X, has_motive(X, vol), R32), write(R32), nl,
    write('33. has_motive(X, assassinat): '), findall(X, has_motive(X, assassinat), R33), write(R33), nl,
    write('34. has_motive(X, escroquerie): '), findall(X, has_motive(X, escroquerie), R34), write(R34), nl,
    write('35. has_motive(john, X): '), findall(X, has_motive(john, X), R35), write(R35), nl,
    write('36. has_motive(mary, X): '), findall(X, has_motive(mary, X), R36), write(R36), nl,
    write('37. has_motive(alice, X): '), findall(X, has_motive(alice, X), R37), write(R37), nl,
    write('38. has_motive(X, Y): '), findall(X-Y, has_motive(X, Y), R38), write(R38), nl,
    write('39. was_near_crime_scene(john, vol): '), (was_near_crime_scene(john, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('40. was_near_crime_scene(mary, assassinat): '), (was_near_crime_scene(mary, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    nl,

    % REQUÊTES 41-60: EMPREINTES ET PREUVES
    write('=== REQUÊTES 41-60: EMPREINTES ET PREUVES ==='), nl,
    write('41. has_fingerprint_on_weapon(john, vol): '), (has_fingerprint_on_weapon(john, vol) -> write('TRUE') ; write('FALSE')), nl,
    write('42. has_fingerprint_on_weapon(mary, assassinat): '), (has_fingerprint_on_weapon(mary, assassinat) -> write('TRUE') ; write('FALSE')), nl,
    write('43. has_fingerprint_on_weapon(X, vol): '), findall(X, has_fingerprint_on_weapon(X, vol), R43), write(R43), nl,
    write('44. has_fingerprint_on_weapon(X, assassinat): '), findall(X, has_fingerprint_on_weapon(X, assassinat), R44), write(R44), nl,
    write('45. has_fingerprint_on_weapon(john, X): '), findall(X, has_fingerprint_on_weapon(john, X), R45), write(R45), nl,
    write('46. has_fingerprint_on_weapon(mary, X): '), findall(X, has_fingerprint_on_weapon(mary, X), R46), write(R46), nl,
    write('47. has_fingerprint_on_weapon(X, Y): '), findall(X-Y, has_fingerprint_on_weapon(X, Y), R47), write(R47), nl,
    write('48. has_bank_transaction(alice, escroquerie): '), (has_bank_transaction(alice, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('49. has_bank_transaction(bruno, escroquerie): '), (has_bank_transaction(bruno, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('50. has_bank_transaction(X, escroquerie): '), findall(X, has_bank_transaction(X, escroquerie), R50), write(R50), nl,
    write('51. has_bank_transaction(alice, X): '), findall(X, has_bank_transaction(alice, X), R51), write(R51), nl,
    write('52. has_bank_transaction(bruno, X): '), findall(X, has_bank_transaction(bruno, X), R52), write(R52), nl,
    write('53. has_bank_transaction(X, Y): '), findall(X-Y, has_bank_transaction(X, Y), R53), write(R53), nl,
    write('54. owns_fake_identity(sophie, escroquerie): '), (owns_fake_identity(sophie, escroquerie) -> write('TRUE') ; write('FALSE')), nl,
    write('55. owns_fake_identity(X, escroquerie): '), findall(X, owns_fake_identity(X, escroquerie), R55), write(R55), nl,
    write('56. owns_fake_identity(sophie, X): '), findall(X, owns_fake_identity(sophie, X), R56), write(R56), nl,
    write('57. owns_fake_identity(X, Y): '), findall(X-Y, owns_fake_identity(X, Y), R57), write(R57), nl,
    write('58. was_near_crime_scene(X, vol): '), findall(X, was_near_crime_scene(X, vol), R58), write(R58), nl,
    write('59. was_near_crime_scene(X, assassinat): '), findall(X, was_near_crime_scene(X, assassinat), R59), write(R59), nl,
    write('60. was_near_crime_scene(X, Y): '), findall(X-Y, was_near_crime_scene(X, Y), R60), write(R60), nl,
    nl,

    % REQUÊTES 61-80: REQUÊTES COMBINÉES
    write('=== REQUÊTES 61-80: REQUÊTES COMBINÉES ==='), nl,
    write('61. has_motive(X,vol) & was_near_crime_scene(X,vol): '),
    findall(X, (has_motive(X, vol), was_near_crime_scene(X, vol)), R61), write(R61), nl,
    write('62. has_motive(X,assassinat) & was_near_crime_scene(X,assassinat): '),
    findall(X, (has_motive(X, assassinat), was_near_crime_scene(X, assassinat)), R62), write(R62), nl,
    write('63. has_motive(X,escroquerie) & has_bank_transaction(X,escroquerie): '),
    findall(X, (has_motive(X, escroquerie), has_bank_transaction(X, escroquerie)), R63), write(R63), nl,
    write('64. has_motive(X,escroquerie) & owns_fake_identity(X,escroquerie): '),
    findall(X, (has_motive(X, escroquerie), owns_fake_identity(X, escroquerie)), R64), write(R64), nl,
    write('65. suspect(X) & has_motive(X,vol): '), findall(X, (suspect(X), has_motive(X, vol)), R65), write(R65), nl,
    write('66. suspect(X) & has_motive(X,assassinat): '), findall(X, (suspect(X), has_motive(X, assassinat)), R66), write(R66), nl,
    write('67. suspect(X) & has_motive(X,escroquerie): '), findall(X, (suspect(X), has_motive(X, escroquerie)), R67), write(R67), nl,
    write('68. suspect(X) & is_guilty(X,vol): '), findall(X, (suspect(X), is_guilty(X, vol)), R68), write(R68), nl,
    write('69. suspect(X) & is_guilty(X,assassinat): '), findall(X, (suspect(X), is_guilty(X, assassinat)), R69), write(R69), nl,
    write('70. suspect(X) & is_guilty(X,escroquerie): '), findall(X, (suspect(X), is_guilty(X, escroquerie)), R70), write(R70), nl,
    write('71. suspect(X) & is_guilty(X,Y): '), findall(X-Y, (suspect(X), is_guilty(X, Y)), R71), write(R71), nl,
    write('72. suspect(X) & NOT is_guilty(X,vol): '), findall(X, (suspect(X), \+ is_guilty(X, vol)), R72), write(R72), nl,
    write('73. suspect(X) & NOT is_guilty(X,assassinat): '), findall(X, (suspect(X), \+ is_guilty(X, assassinat)), R73), write(R73), nl,
    write('74. suspect(X) & NOT is_guilty(X,escroquerie): '), findall(X, (suspect(X), \+ is_guilty(X, escroquerie)), R74), write(R74), nl,
    write('75. suspect(X) & NOT is_guilty(X,_): '), findall(X, (suspect(X), \+ is_guilty(X, _)), R75), write(R75), nl,
    write('76. has_motive(X,Y) & NOT is_guilty(X,Y): '), findall(X-Y, (has_motive(X, Y), \+ is_guilty(X, Y)), R76), write(R76), nl,
    write('77. was_near_crime_scene(X,Y) & NOT is_guilty(X,Y): '), findall(X-Y, (was_near_crime_scene(X, Y), \+ is_guilty(X, Y)), R77), write(R77), nl,
    write('78. has_fingerprint_on_weapon(X,Y) & is_guilty(X,Y): '), findall(X-Y, (has_fingerprint_on_weapon(X, Y), is_guilty(X, Y)), R78), write(R78), nl,
    write('79. has_bank_transaction(X,Y) & is_guilty(X,Y): '), findall(X-Y, (has_bank_transaction(X, Y), is_guilty(X, Y)), R79), write(R79), nl,
    write('80. owns_fake_identity(X,Y) & is_guilty(X,Y): '), findall(X-Y, (owns_fake_identity(X, Y), is_guilty(X, Y)), R80), write(R80), nl,
    nl,

    % REQUÊTES 81-100: REQUÊTES AVANCÉES
    write('=== REQUÊTES 81-100: REQUÊTES AVANCÉES ==='), nl,
    write('81. findall(X, suspect(X), Suspects): '), findall(X, suspect(X), R81), write(R81), nl,
    write('82. findall(X, crime_type(X), Crimes): '), findall(X, crime_type(X), R82), write(R82), nl,
    write('83. findall(X, is_guilty(X,vol), VolCoupables): '), findall(X, is_guilty(X, vol), R83), write(R83), nl,
    write('84. findall(X, is_guilty(X,assassinat), AssassinatCoupables): '), findall(X, is_guilty(X, assassinat), R84), write(R84), nl,
    write('85. findall(X, is_guilty(X,escroquerie), EscroquerieCoupables): '), findall(X, is_guilty(X, escroquerie), R85), write(R85), nl,
    write('86. findall(X-Y, is_guilty(X,Y), TousCoupables): '), findall(X-Y, is_guilty(X, Y), R86), write(R86), nl,
    write('87. findall(X, has_motive(X,vol), MotifsVol): '), findall(X, has_motive(X, vol), R87), write(R87), nl,
    write('88. findall(X, has_motive(X,assassinat), MotifsAssassinat): '), findall(X, has_motive(X, assassinat), R88), write(R88), nl,
    write('89. findall(X, has_motive(X,escroquerie), MotifsEscroquerie): '), findall(X, has_motive(X, escroquerie), R89), write(R89), nl,
    write('90. findall(X-Y, has_motive(X,Y), TousMotifs): '), findall(X-Y, has_motive(X, Y), R90), write(R90), nl,
    write('91. Tous les suspects avec leur statut: '), nl,
    forall(suspect(X), (write('   '), write(X), write(' est suspect'), nl)),
    write('92. Tous les coupables avec leurs crimes: '), nl,
    forall(is_guilty(X, Y), (write('   '), write(X), write(' coupable de '), write(Y), nl)),
    write('93. bagof pour vol (si existe): '),
    (bagof(X, is_guilty(X, vol), R93) -> write(R93) ; write('aucun')), nl,
    write('94. bagof pour assassinat (si existe): '),
    (bagof(X, is_guilty(X, assassinat), R94) -> write(R94) ; write('aucun')), nl,
    write('95. bagof pour escroquerie (si existe): '),
    (bagof(X, is_guilty(X, escroquerie), R95) -> write(R95) ; write('aucun')), nl,
    write('96. setof tous coupables uniques: '),
    (setof(X, Y^is_guilty(X, Y), R96) -> write(R96) ; write('aucun')), nl,
    write('97. setof crimes avec coupables: '),
    (setof(Y, X^is_guilty(X, Y), R97) -> write(R97) ; write('aucun')), nl,
    write('98. Count vol coupables: '),
    aggregate_all(count, is_guilty(_, vol), R98), write(R98), nl,
    write('99. Count total coupables: '),
    aggregate_all(count, is_guilty(_, _), R99), write(R99), nl,
    write('100. Premier coupable trouvé: '),
    (once(is_guilty(X, Y)) -> write(X-Y) ; write('aucun')), nl,

    nl,
    write('=========================================='), nl,
    write('FIN DE L\'EXÉCUTION DES 100 REQUÊTES'), nl,
    write('=========================================='), nl.

% Lancement automatique au démarrage
:- initialization(run_all_100_queries).
