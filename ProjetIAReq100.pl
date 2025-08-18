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

% === REQUÊTES DE TEST POUR VALIDATION ===
test_100_queries :-
    write('=== TEST DES 100 REQUÊTES ==='), nl,

    % Test requêtes de base
    write('1. is_guilty(john, vol): '),
    (is_guilty(john, vol) -> write('true') ; write('false')), nl,

    write('2. is_guilty(mary, assassinat): '),
    (is_guilty(mary, assassinat) -> write('true') ; write('false')), nl,

    write('3. is_guilty(alice, escroquerie): '),
    (is_guilty(alice, escroquerie) -> write('true') ; write('false')), nl,

    % Test requêtes avec variables
    write('16. Coupables de vol: '),
    findall(X, is_guilty(X, vol), VolCoupables),
    write(VolCoupables), nl,

    write('17. Coupables d''assassinat: '),
    findall(X, is_guilty(X, assassinat), AssassinatCoupables),
    write(AssassinatCoupables), nl,

    write('18. Coupables d''escroquerie: '),
    findall(X, is_guilty(X, escroquerie), EscroquerieCoupables),
    write(EscroquerieCoupables), nl,

    % Test tous coupables
    write('86. Tous les coupables: '),
    findall(X-Y, is_guilty(X, Y), TousCoupables),
    write(TousCoupables), nl,

    write('=== FIN DES TESTS ==='), nl.

% Pour exécuter le test:
% ?- test_100_queries.
