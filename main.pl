/*Dynamic Declaration*/
:- dynamic(player_pos/2).
:- dynamic(player_health/1).
:- dynamic(player_armor/1).
:- dynamic(player_weapon/1).
:- dynamic(player_ammo/1).
:- dynamic(player_inv/1).
:- dynamic(in_inv/1).
:- dynamic(weapon_pos/3).
:- dynamic(ammo_pos/3).
:- dynamic(armor_pos/3).
:- dynamic(med_pos/3).
:- dynamic(deadzone_size/1).
:- dynamic(deadzone_timer/1).
:- dynamic(enemy_pos/3).
:- dynamic(enemy_weapon/2).
:- dynamic(enemy_health/2).
:- dynamic(enemyList/1).

/*Deklarasi rule player*/
start :-
  print_title,
  helpcmd,
  initPlayer,
  initEnemy,
  initItem,
  asserta(deadzone_timer(5)), !,
  repeat,
    write('$-'),
    read(In),
    exec(In),
    deadzone_tick,
    enemyTick, nl,
  (In == exit; endGame).

/*Command execution alias*/
exec(map) :- map, !.
exec(look) :- look, !.
exec(att) :- attack, enemy_attack,!.
exec(stat) :- status,!.
exec(n) :- move(0,-1),enemyList(L), enemyMove(L), !.
exec(e) :- move(1,0), enemyList(L), enemyMove(L), !.
exec(s) :- move(0,1), enemyList(L), enemyMove(L), !.
exec(w) :- move(-1,0),enemyList(L), enemyMove(L), !.
exec(help) :- helpcmd, !.
exec(exit) :- write('Yah kok udahan :('),!.
exec(_) :- write('Invalid command, kan programnya belum kelar :('), !.

/*Miscelanious*/
print_title :-
  write('= WELCOME TO SKIDIPAPAP UNKNOWN BATTLEGROUND ='),nl,
  write('      ███████╗██╗   ██╗██████╗  ██████╗ '),nl,
  write('      ██╔════╝██║   ██║██╔══██╗██╔════╝ '),nl,
  write('      ███████╗██║   ██║██████╔╝██║  ███╗'),nl,
  write('      ╚════██║██║   ██║██╔══██╗██║   ██║'),nl,
  write('      ███████║╚██████╔╝██████╔╝╚██████╔╝'),nl,
  write('      ╚══════╝ ╚═════╝ ╚═════╝  ╚═════╝ '),nl,
  write('======== LOOT , SCOOP and SKIDIPAPAP ========'),nl,nl.

helpcmd :-
  write('Currently usable command:'),nl,
  write('  $- map  : print the whole map'),nl,
  write('  $- look : look around'),nl,
  write('  $- att  : attack enemy in the same grid'),nl,
  write('  $- stat : show current status'),nl,
  write('  $- n    : move north'),nl,
  write('  $- e    : move east'),nl,
  write('  $- s    : move south'),nl,
  write('  $- w    : move west'),nl,
  write('  $- help : show this help'),nl,
  write('  $- exit : exit the game'),nl.

/*Endgame Conditions*/
endGame :-
  player_pos(X,Y),
  deadzone_size(V),
  (X@=<V; Y@=<V; Vright is 11-V ,X@>=Vright; Vright is 11-V ,Y@>=Vright), !,
  write('Skidipapman kehabisan darah dan mati di deadzone'),nl.

endGame :-
  player_health(X),
  X==0, !,
  write('Skidipapman mati karena luka peperangan'),nl.

endGame :-
  enemyList([]), !,
  write('Selamat Skidipapman berhasil bertahan hidup'),nl.

/*Game initialization*/
initPlayer :-
  asserta(player_pos(5,5)),
  asserta(player_health(100)),
  asserta(player_armor(0)),
  asserta(player_ammo(5)),
  asserta(player_inv(15)),
  asserta(player_weapon(awm)).

initItem :-
  asserta(weapon_pos(1,1,awm)),
  asserta(ammo_pos(1,3,sniper_ammo)),
  asserta(armor_pos(3,4,helm_spetsnaz)),
  asserta(med_pos(3,3,med_kit)),
  asserta(deadzone_size(0)).

/*Deklarasi objek/item*/
weapon_obj(awm).
weapon_obj(m24).
weapon_obj(akm).
weapon_obj(p18c).
weapon_obj(m1997).
weapon_type(awm,sniper).
weapon_type(m24,smg).
weapon_type(akm,ar).
weapon_type(m1997,sg).
weapon_type(p18c,pistol).
weapon_dmg(awm,70).
weapon_dmg(m24,25).
weapon_dmg(akm,35).
weapon_dmg(p18c,10).
weapon_dmg(m1997,60).

ammo_obj(sniper_ammo).
ammo_obj(smg_ammo).
ammo_obj(ar_ammo).
ammo_obj(pistol_ammo).
ammo_type(sniper_ammo,sniper).
ammo_type(smg_ammo,smg).
ammo_type(ar_ammo,ar).
ammo_type(sg_ammo,sg).
ammo_type(pistol_ammo,pistol).
ammo_count(sniper_ammo,5).
ammo_count(smg_ammo,30).
ammo_count(pistol_ammo,40).
ammo_count(ar_ammo,20).
ammo_count(sg_ammo,10).

armor_obj(helm_spetsnaz).
armor_obj(helm_military).
armor_obj(vest_military).
armor_obj(vest_police).
armor_amount(helm_spetsnaz,30).
armor_amount(helm_military,15).
armor_amount(vest_military,50).
armor_amount(vest_police,30).

med_obj(bandages).
med_obj(med_kit).
med_obj(first_aid_kit).
med_heal(bandages,30).
med_heal(med_kit,50).
med_heal(first_aid_kit,80).

/*Player status*/
status :-
  player_health(H), player_armor(Ar), player_weapon(W), player_ammo(Am), weapon_dmg(W,Dmg),
  write('Health : '), write(H), nl,
  write('Armor  : '), write(Ar), nl,
  write('Equipped Weapon : '), write(W), write(' ('), write(Dmg) , write(')'), nl,
  write('Ammo   : '), write(Am),nl, !.

/*Deklarasi dan rule terkait map*/

/*Konvensi : Peta dimulai di kiri atas pada koordinat (0,0).
             Nilai X membesar ke kanan.
             Nilai Y membesar ke bawah*/
/*Deadzone*/
print_pos(X,Y) :-
  deadzone_size(V),
  (X@=<V; Y@=<V; Vright is 11-V ,X@>=Vright; Vright is 11-V ,Y@>=Vright), !,
  write('X').
/*Enemy*/
print_pos(X,Y) :-
  enemy_pos(_,A,B),
  A==X, B==Y, !,
  write('E').
/*Medicine*/
print_pos(X,Y) :-
  med_pos(A,B,_),
  A==X, B==Y, !,
  write('M').
/*Weapon*/
print_pos(X,Y) :-
  weapon_pos(A,B,_),
  A==X, B==Y, !,
  write('W').
/*Armor*/
print_pos(X,Y) :-
  armor_pos(A,B,_),
  A==X, B==Y, !,
  write('A').
/*Ammo*/
print_pos(X,Y) :-
  ammo_pos(A,B,_),
  A==X, B==Y, !,
  write('+').
/*Player*/
print_pos(X,Y) :-
  player_pos(A,B),
  A==X, B==Y, !,
  write('P').
/*Regular tiles*/
print_pos(_,_) :- write('-').

print_map(11,11) :- print_pos(11,11),nl,!.
print_map(11,Y) :- Ynew is Y+1, print_pos(11,Y),nl,!,print_map(0,Ynew).
print_map(X,Y) :-
  deadzone_size(V),
  (X@=<V; Y@=<V; Vright is 11-V ,X@>=Vright; Vright is 11-V ,Y@>=Vright), !,
  write('X'),
  Xnew is X+1, print_map(Xnew,Y).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == Xmin, B == Ymin,
  print_pos(Xmin,Ymin), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == X, B == Ymin,
  print_pos(X,Ymin), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == Xplus, B == Ymin,
  print_pos(Xplus,Ymin), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == Xmin, B == Y,
  print_pos(Xmin,Y), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == X, B == Y,
  print_pos(X,Y), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == Xplus, B == Y,
  print_pos(Xplus,Y), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == Xmin, B == Yplus,
  print_pos(Xmin,Yplus), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == X, B == Yplus,
  print_pos(X,Yplus), !,
  Anew is A+1, print_map(Anew,B).
print_map(A,B) :-
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  A == Xplus, B == Yplus,
  print_pos(Xplus,Yplus), !,
  Anew is A+1, print_map(Anew,B).
print_map(X,Y) :- Xnew is X+1, write('-'),!,print_map(Xnew,Y).

deadzone_tick :-
  deadzone_timer(T), T == 0, !,
  retract(deadzone_timer(T)), asserta(deadzone_timer(6)),
  deadzone_size(X), Xnew is X+1, retract(deadzone_size(X)), assertz(deadzone_size(Xnew)).

deadzone_tick :- !.

map :-
  write('You are in pochinki'),nl,
  write('Tiles info:'),nl,
  write(' [X] Deadzone  | [P] Player'),nl,
  write(' [M] Medicine  | [A] Armor'),nl,
  write(' [W] Weapon    | [+] Ammo'),nl,
  print_map(0,0).

look :-
  write('You look your surrounding'),nl,
  write('Tiles info:'),nl,
  write(' [X] Deadzone  | [P] Player'),nl,
  write(' [M] Medicine  | [A] Armor'),nl,
  write(' [W] Weapon    | [+] Ammo'),nl,nl,
  player_pos(X,Y),
  Xmin is X-1, Xplus is X+1, Ymin is Y-1, Yplus is Y+1,
  write(' '),print_pos(Xmin,Ymin), print_pos(X,Ymin), print_pos(Xplus,Ymin), nl,
  write(' '),print_pos(Xmin,Y), print_pos(X,Y), print_pos(Xplus,Y), nl,
  write(' '),print_pos(Xmin,Yplus), print_pos(X,Yplus), print_pos(Xplus,Yplus).

/*Deklarasi dan rule terkait movement player*/
is_valid_move(X,_) :- deadzone_size(V), X@=<V, !, fail.
is_valid_move(_,Y) :- deadzone_size(V), Y@=<V, !, fail.
is_valid_move(X,_) :- deadzone_size(V), Vright is 11-V ,X@>=Vright, !, fail.
is_valid_move(_,Y) :- deadzone_size(V), Vright is 11-V ,Y@>=Vright, !, fail.
is_valid_move(_,_).

move(X,Y) :-
  player_pos(A,B), Xmov is X+A, Ymov is Y+B, is_valid_move(Xmov,Ymov), !,
  deadzone_timer(T),
  Tnew is T-1, retract(deadzone_timer(T)), asserta(deadzone_timer(Tnew)),
  (Tnew == 0, write('Deadzone is shrinking'), nl; write(Tnew), write(' tick to deadzone shrinking'), nl), !,
  retract(player_pos(A,B)), assertz(player_pos(Xmov,Ymov)).

/*Rule untuk invalid move*/
move(_,_) :- write('You cannot go into the deadzone, dumbass'),nl.

/* Enemy */
initEnemy :-
  asserta(enemyList([asrap,bari,badur,jeremy,rojap,abiyyu,suhailie,joshua,cici,pandyaka])),
  enemyList(L),
  initEnemyPos(L),
  initEnemyWeapon.

initEnemyPos([H|T]):-
  randomize,
  random(1,10,PosX),
  random(1,10,PosY),
  assertz(enemy_pos(H,PosX,PosY)),
  assertz(enemy_health(H,100)),
  initEnemyPos(T).

initEnemyPos([]).

initEnemyWeapon :-
  assertz(enemy_weapon(asrap,awm)),
  assertz(enemy_weapon(bari,p18c)),
  assertz(enemy_weapon(badur,m24)),
  assertz(enemy_weapon(jeremy,p18c)),
  assertz(enemy_weapon(rojap,akm)),
  assertz(enemy_weapon(abiyyu,m1997)),
  assertz(enemy_weapon(suhailie,akm)),
  assertz(enemy_weapon(joshua,awm)),
  assertz(enemy_weapon(cici,m1997)),
  assertz(enemy_weapon(pandyaka,p18c)).

/*Gerak musuh*/
enemyMove([H|T]) :-
  randomize,
  random(1,4,MovDir),
  enemy_pos(H,X,Y),
  (
    (MovDir==1, Xnew is X+1, retract(enemy_pos(H,X,Y)), assertz(enemy_pos(H,Xnew,Y)));
    (MovDir==2, Xnew is X-1, retract(enemy_pos(H,X,Y)), assertz(enemy_pos(H,Xnew,Y)));
    (MovDir==3, Ynew is Y+1, retract(enemy_pos(H,X,Y)), assertz(enemy_pos(H,X,Ynew)));
    (MovDir==4, Ynew is Y-1, retract(enemy_pos(H,X,Y)), assertz(enemy_pos(H,X,Ynew)))
  ),!,enemyMove(T).

enemyMove([]).

/*Cek kematian musuh*/
enemyTick :-
  enemyList(L),
  checkDeath(L).

checkDeath([H|T]):-
  enemy_health(H,CurrHt),
  CurrHt@>=0, !,
  checkDeath(T).

checkDeath([H|T]):-
  enemyList(L),
  enemy_health(H,CurrHt), enemy_pos(H,X,Y), deadzone_size(V), !,
  (CurrHt@=<0 ; (X@=<V; Y@=<V; Vright is 11-V ,X@>=Vright; Vright is 11-V ,Y@>=Vright)),
  delete(L, H, Lnew),
  retract(enemyList(L)), asserta(enemyList(Lnew)),
  retract(enemy_pos(H,X,Y)),
  retract(enemy_weapon(H,W)),
  retract(enemy_health(H,CurrHt)),
  assertz(weapon_pos(X,Y,W)), !,
  checkDeath(T).

checkDeath([]).

/* Combat mechanic */
attack :-
  player_pos(X,Y),
  enemy_pos(H,A,B),
  X==A, Y==B,
  player_weapon(V), player_ammo(N),
  V\==none, N@>0,
  enemy_health(H,Ht), weapon_dmg(V,Dmg),
  Htnew is Ht-Dmg, Htnew@>0, !,
  retract(enemy_health(H,Ht)), asserta(enemy_health(H,Htnew)),
  Nnew is N-1, retract(player_ammo(N)), asserta(player_ammo(Nnew)),
  write('You attacked '),write(H),write(', Remaining HP:'),write(Htnew), nl,!.

attack :-
  player_pos(X,Y),
  enemy_pos(H,A,B),
  X==A, Y==B, !,
  player_weapon(V), player_ammo(N),
  V\==none, N@>0, !,
  enemy_health(H,Ht), weapon_dmg(V,Dmg),
  Htnew is Ht-Dmg, Htnew@=<0, !,
  retract(enemy_health(H,Ht)), asserta(enemy_health(H,Htnew)),
  Nnew is N-1, retract(player_ammo(N)), asserta(player_ammo(Nnew)),
  enemyTick,
  write('You killed '),write(H), nl,!.

attack :-
  player_pos(X,Y),
  enemy_pos(H,A,B),
  X==A, Y==B, !,
  player_weapon(V), player_ammo(N),
  V\==none, N==0, !,
  write('You have no ammo'), nl,!.

attack :-
  player_pos(X,Y),
  enemy_pos(H,A,B),
  X==A, Y==B, !,
  player_weapon(V),
  V==none, !,
  write('You have no weapon'), nl,!.

attack :-
  write('There is no one to attack'), nl,!.

enemy_attack :-
  player_pos(X,Y),
  enemy_pos(H,A,B),
  X==A, Y==B, !,
  enemy_weapon(H,W), weapon_dmg(W,Dmg),
  player_health(Ht), player_armor(Arm),!,
  (Arm@>0, (NewArm is Arm-Dmg, !,
           (NewArm@>=0, retract(player_armor(Arm)), asserta(player_armor(NewArm)),
           write('Your current armor: '),write(NewArm));
           (NewArm@<0, retract(player_armor(Arm)), asserta(player_armor(0)),
           Htnew is Ht+NewArm, retract(player_health(Ht)), asserta(player_health(Htnew)),
           write('Your current health: '),write(Htnew)));
  Arm==0, (Htnew is Ht-Dmg, !,
          (Htnew@>0, retract(player_health(Ht)), asserta(player_health(Htnew)),
          write('Your current health: '),write(Htnew));
          (Htnew@=<0, retract(player_health(Ht)), asserta(player_health(0))))),nl,!.

enemy_attack.
