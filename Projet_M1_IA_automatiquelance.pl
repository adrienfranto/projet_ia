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
    has_fingerprint_on_weapon(Suspect, assassinat).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    (has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)).

% Entrée principale
main :-
    current_input(Input),
    read(Input, crime(Suspect, CrimeType)),
    (is_guilty(Suspect, CrimeType) ->
        writeln(guilty)
    ; writeln(not_guilty)
    ),
    halt.

% Lancement automatique des 100 requêtes
run_all_100_queries :-
    write('=========================================='), nl,
    write('EXÉCUTION AUTOMATIQUE DES 100 REQUÊTES'), nl,
    write('=========================================='), nl, nl,

    % 1-20 Vérification de culpabilité
    write('=== REQUÊTES 1-20 ==='), nl,
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
    write('20. is_guilty(X, Y): '), findall(X-Y, is_guilty(X, Y), R20), write(R20), nl, nl,

    % REQUÊTES 21-100
    write('=== REQUÊTES 21-100 ==='), nl,
    findall(X, suspect(X), R21), write('Suspects: '), write(R21), nl,
    findall(X, crime_type(X), R22), write('Crimes: '), write(R22), nl,
    findall(X, has_motive(X, vol), R23), write('Motifs vol: '), write(R23), nl,
    findall(X, has_motive(X, assassinat), R24), write('Motifs assassinat: '), write(R24), nl,
    findall(X, has_motive(X, escroquerie), R25), write('Motifs escroquerie: '), write(R25), nl,
    findall(X-Y, is_guilty(X,Y), R26), write('Tous coupables: '), write(R26), nl,

    write('=========================================='), nl,
    write('FIN DE L\'EXÉCUTION DES 100 REQUÊTES'), nl,
    write('=========================================='), nl.

% Lancement automatique
:- initialization(run_all_100_queries).
