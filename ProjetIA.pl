% Consigne projet IA MI G5 IG
% Projet basé sur la programmation logique (PROLOG)
% Système d'enquête policière (ou judiciaire)
% Deadline: 24 Août 2025

% Types de crime
crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

% Faits - Suspects
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% Faits - Motifs
has_motive(john, vol).
has_motive(mary, assassinat).
has_motive(alice, escroquerie).
has_motive(bruno, escroquerie).
has_motive(sophie, vol).

% Faits - Présence sur les lieux du crime
was_near_crime_scene(john, vol).
was_near_crime_scene(mary, assassinat).
was_near_crime_scene(alice, vol).
was_near_crime_scene(bruno, vol).

% Faits - Empreintes sur l'arme
has_fingerprint_on_weapon(john, vol).
has_fingerprint_on_weapon(mary, assassinat).
has_fingerprint_on_weapon(alice, escroquerie).

% Faits - Transactions bancaires suspectes
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).
has_bank_transaction(sophie, vol).

% Faits - Fausses identités
owns_fake_identity(sophie, escroquerie).
owns_fake_identity(bruno, escroquerie).

% Faits - Témoignages oculaires
eyewitness_identification(john, vol).
eyewitness_identification(mary, assassinat).
eyewitness_identification(alice, escroquerie).

% Règles de culpabilité

% Règle pour vol
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    (has_fingerprint_on_weapon(Suspect, vol)
    ; eyewitness_identification(Suspect, vol)
    ; has_bank_transaction(Suspect, vol)).

% Règle pour assassinat
is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    (has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)).

% Règle pour escroquerie
is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    (has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ; eyewitness_identification(Suspect, escroquerie)).

% Règle générale - un suspect est innocent s'il n'est pas coupable
is_innocent(Suspect, CrimeType) :-
    suspect(Suspect),
    crime_type(CrimeType),
    \+ is_guilty(Suspect, CrimeType).

% Prédicats utilitaires

% Trouver tous les suspects coupables d'un crime
find_guilty_suspects(CrimeType, Suspects) :-
    findall(Suspect, is_guilty(Suspect, CrimeType), Suspects).

% Trouver tous les crimes dont un suspect est coupable
find_crimes_by_suspect(Suspect, Crimes) :-
    findall(Crime, is_guilty(Suspect, Crime), Crimes).

% Compter le nombre d'indices contre un suspect pour un crime
count_evidence(Suspect, CrimeType, Count) :-
    findall(1, evidence_against(Suspect, CrimeType), Evidence),
    length(Evidence, Count).

% Définir ce qui constitue un indice
evidence_against(Suspect, CrimeType) :-
    has_motive(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    was_near_crime_scene(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    has_fingerprint_on_weapon(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    eyewitness_identification(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    has_bank_transaction(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    owns_fake_identity(Suspect, CrimeType).

% Afficher un résumé de l'enquête
show_investigation_summary :-
    write('=== RÉSUMÉ DE L\'ENQUÊTE ==='), nl, nl,
    crime_type(Crime),
    write('Crime: '), write(Crime), nl,
    find_guilty_suspects(Crime, Guilty),
    (Guilty = [] ->
        write('Aucun suspect coupable trouvé.'), nl
    ;
        write('Suspects coupables: '), write(Guilty), nl
    ),
    nl,
    fail.
show_investigation_summary.

% Point d'entrée principal
main :-
    write('Système d\'enquête policière'), nl,
    write('================================'), nl, nl,
    current_input(Input),
    write('Entrez le nom du suspect et le type de crime (ex: john, vol): '),
    read(Input, crime(Suspect, CrimeType)),
    (is_guilty(Suspect, CrimeType) ->
        write('Résultat: COUPABLE'), nl,
        write(Suspect), write(' est coupable de '), write(CrimeType), nl
    ;
        write('Résultat: NON COUPABLE'), nl,
        write(Suspect), write(' n\'est pas coupable de '), write(CrimeType), nl
    ),
    halt.

