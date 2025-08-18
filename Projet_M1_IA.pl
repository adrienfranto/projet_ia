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

% Exemple à reprendre
% ?- is_guilty(john, vol).
% ?- is_guilty(mary, assassinat).
% ?- is_guilty(alice, escroquerie).
