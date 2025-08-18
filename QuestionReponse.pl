% ==============================
% PROJET ENQUÊTE POLICIÈRE - EXECUTION AUTOMATIQUE
% ==============================

% Inclure ici tout ton code précédent (suspects, faits, règles, etc.)

% Exemple minimal pour illustration
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).

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

is_guilty(S, vol) :-
    has_motive(S, vol),
    was_near_crime_scene(S, vol),
    has_fingerprint_on_weapon(S, vol).

is_guilty(S, assassinat) :-
    has_motive(S, assassinat),
    was_near_crime_scene(S, assassinat),
    has_fingerprint_on_weapon(S, assassinat).

is_guilty(S, escroquerie) :-
    has_motive(S, escroquerie),
    (has_bank_transaction(S, escroquerie)
    ; owns_fake_identity(S, escroquerie)).

% ==============================
% EXECUTION AUTOMATIQUE DES QUESTIONS
% ==============================

run_all_queries :-
    write('=== EXECUTION AUTOMATIQUE DES REQUETES ==='), nl, nl,

    write('1. John coupable de vol ? '),
    (is_guilty(john, vol) -> writeln(oui) ; writeln(non)),

    write('2. Mary coupable d\'assassinat ? '),
    (is_guilty(mary, assassinat) -> writeln(oui) ; writeln(non)),

    write('3. Alice coupable d\'escroquerie ? '),
    (is_guilty(alice, escroquerie) -> writeln(oui) ; writeln(non)),

    write('4. Bruno coupable d\'escroquerie ? '),
    (is_guilty(bruno, escroquerie) -> writeln(oui) ; writeln(non)),

    write('5. Sophie coupable d\'escroquerie ? '),
    (is_guilty(sophie, escroquerie) -> writeln(oui) ; writeln(non)),

    write('6. Tous les coupables de vol: '),
    findall(X, is_guilty(X, vol), V),
    writeln(V),

    write('7. Tous les coupables d\'assassinat: '),
    findall(X, is_guilty(X, assassinat), A),
    writeln(A),

    write('8. Tous les coupables d\'escroquerie: '),
    findall(X, is_guilty(X, escroquerie), E),
    writeln(E),

    write('=== FIN DE L\'EXECUTION AUTOMATIQUE ==='), nl.

% Pour lancer tout automatiquement :
% ?- run_all_queries.
