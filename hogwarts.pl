
% the students in Hogwarts
student(hp, 'Harry James Potter', gryffindor).
student(hg, 'Hermione Jean Granger', gryffindor).
student(rw, 'Ronald Weasley', gryffindor).
student(ll, 'Luna Lovegood', ravenclaw).
student(cc, 'Cho Chang', ravenclaw).
student(tb, 'Terry Boot', ravenclaw).
student(ic, 'Izaak Coleman', hufflepuff).
student(ha, 'Hannah Abbott', hufflepuff).
student(cd, 'Cedric Diggory', hufflepuff).
student(nt, 'Nymphadora Tonks',hufflepuff).
student(dm, 'Draco Malfoy', slytherin).
student(gg, 'Gregory Goyle', slytherin).
student(vc, 'Vincent Crabbe', slytherin).

% the teachers in Hogwarts
teacher(ad, 'Albus Percival Wulfric Brian Dumbledore').
teacher(ff, 'Filius Flitwick').
teacher(rh, 'Rubeus Hagrid').
teacher(gl, 'Gilderoy Lockhart').
teacher(rl, 'Remus John Lupin').
teacher(mm, 'Minerva McGonagall').
teacher(qq, 'Quirinus Quirrell').
teacher(ss, 'Severus Snape').
teacher(ps, 'Pomona Sprout').
teacher(st, 'Sibyll Patricia Trelawney').
teacher(mh, 'Madam Hooch').
teacher(as, 'Aurora Sinistra').
teacher(cub, 'Cuthbert Binns').
teacher(bb, 'Bathsheba Babbling').
teacher(sv, 'Septima Vector').
teacher(chb, 'Charity Burbage').
teacher(wt, 'Wilkie Twycross').

%houses at hogwarts
house(gryffindor).
house(hufflepuff).
house(ravenclaw).
house(slytherin).

% compulsory courses for the MSc in Magic
compCourse(astro, 'Astronomy', as).
compCourse(charms, 'Charms', ff).
compCourse(defence, 'Defence against the Dark Arts', qq).
compCourse(fly, 'Flying', mh).
compCourse(herb, 'Herbology', ps).
compCourse(history, 'History of Magic', cub).
compCourse(potions, 'Potions', ss).
compCourse(trans, 'Transfiguration', mm).

% optional courses for the MSc in Magic
optCourse(runes, 'Study of Ancient Runes', bb).
optCourse(arith, 'Arithmancy', sv).
optCourse(muggle, 'Muggle Studies', chb).
optCourse(creatures, 'Care of Magical Creatures', rh).
optCourse(div, 'Divination', st).
optCourse(app, 'Apparition', wt).
optCourse(choir, 'Frog Choir', ff).
optCourse(quid, 'Quidditch', mh).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optional courses taken by each student. Students take a minimum of 3 courses.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Potter opts
enrolled_opt(hp, div).
enrolled_opt(hp, choir).
enrolled_opt(hp, muggle).


% Granger opts
enrolled_opt(hg, muggle).
enrolled_opt(hg, runes).
enrolled_opt(hg, arith).
enrolled_opt(hg, div).
enrolled_opt(hg, quid).
enrolled_opt(hg, creatures).

% Weasley opts
enrolled_opt(rw, choir).
enrolled_opt(rw, muggle).
enrolled_opt(rw, div).

% Lovejoy opts
enrolled_opt(ll, choir).
enrolled_opt(ll, creatures).
enrolled_opt(ll, arith).

% Chang opts
enrolled_opt(cc, runes).
enrolled_opt(cc, muggle).
enrolled_opt(cc, choir).

% Boot opts
enrolled_opt(tb, app).
enrolled_opt(tb, quid).
enrolled_opt(tb, runes).

% Coleman opts (NB: Was going to take 6 opts, but got lost in Chamber of Secrets
% during term.)
enrolled_opt(ic, runes).
enrolled_opt(ic, app).
enrolled_opt(ic, arith).

% Abbott opts
enrolled_opt(ha, muggle).
enrolled_opt(ha, quid).
enrolled_opt(ha, creatures).

% Diggory opts
enrolled_opt(cd, muggle).
enrolled_opt(cd, arith).
enrolled_opt(cd, choir).


% Tonks opts
enrolled_opt(nt, arith).
enrolled_opt(nt, quid).
enrolled_opt(nt, app).

% Malfoy opts
enrolled_opt(dm, choir).
enrolled_opt(dm, quid).
enrolled_opt(dm, muggle).

% Goyle opts
enrolled_opt(gg, muggle).
enrolled_opt(gg, runes).
enrolled_opt(gg, quid).

% Crabbe opts
enrolled_opt(vc, muggle).
enrolled_opt(vc, arith).
enrolled_opt(vc, app).


% enrolled predicate: Predicate answers relation of which student is taking
% which course. 
enrolled( SID, SCN ):-
  student( SID, _, _), (compCourse( SCN, _, _); enrolled_opt( SID, SCN )).


% teaches predicate: Predicate answers relation of which teacher teaches
% which course. 
teaches( TN, SCN):-
  teacher( TID, TN), (compCourse( SCN, _, TID) ; optCourse( SCN, _, TID)).


% taughtBy predicate: Predicate answers relation of which student is taught
% by which teachers. Query must be student full name, and teacher full name.
taughtBy( SN, TN):-
  student( SID, SN, _), enrolled( SID, SCN), teaches( TN, SCN).


% takesOption predicate: Predicate answers relation of which student
% takes which optional course.
takesOption( SN, CN):-
  student( SID, SN, _), enrolled_opt( SID, SCN ), optCourse( SCN, CN, _).


% takesAllCourses predicate: Predicate relates all the courses a student takes
% to the name of the student.
takesAllOptions( SN, OptCourses) :-
  student( _, SN, _), setof( CN, takesOption( SN, CN), OptCourses).



% studentLoop predicate: Recurses through a list of SID and generates an 
% list of SN for each corrsponding SID in student(SN, SID _).
inhouse(SID, H):-
  student(SID, _, H).

snFromSID( [], []).

snFromSID( [SID_H|SID_T], [SN_H|SN_T]):-
  student( SID_H, SN, _), SN_H = SN, snFromSID( SID_T, SN_T).

studentsInHouse(H, Students):-
  setof(SID, inhouse(SID, H), SIDList), snFromSID( SIDList, Students).

% studensOnCourse

setOfList( [], [] ).
setOfList( [H|T], [H2|T2] ):-
  member(H, T), setOfList( T, [H2|T2]);
  H2=H, setOfList(T, T2).

studentsFromHouses( [], [] ). % get the list of houses for course
studentsFromHouses( [CStu_H|CStu_T], [HousesH|HousesT] ):-
  student(CStu_H, _, House), HousesH = House, 
  studentsFromHouses( CStu_T, HousesT). 

courseStudents_SID( SCN, CourseStudents ):-
  bagof( Student, enrolled( Student, SCN), CourseStudents).

housesOnCourse( SCN, HousesSet):-
  courseStudents_SID( SCN, CourseStudents), 
  studentsFromHouses( CourseStudents, HousesList),
  setOfList( HousesList, HousesSet),!.

checkHouse( SN, H ):-
  student(_, SN, H).

houses( Houses ):-
  setof( House, house(House), Houses).

courseStudents_SN( SCN, CourseStudents ):-
  courseStudents_SID( SCN, SIDList), snFromSID( SIDList, CourseStudents).

houseStudents( House, SCN, Students ):-
  findall( SN, ( courseStudents_SN( SCN, CourseStudents), 
                    member( SN, CourseStudents ), 
                    checkHouse( SN, House)
             ),
             Students).

studentsOnCourse( SCN, CN, StudentsByHouse ):-
  ( compCourse( SCN, CN, _ ) ; optCourse( SCN, CN, _) ),
  findall( House-Students, ( houses( HouseSet ), 
                           member( House, HouseSet), 
                           houseStudents( House, SCN, Students) ), StudentsByHouse).


% sharedCourse
sharedCourse( SN1, SN2, CN):-
  takesAllOptions( SN1, SN1Opts), 
  member( SN1Op, SN1Opts), 
  (student( SID2, SN2, _),
  optCourse( SCN2, SN1Op, _),
  enrolled_opt( SID2, SCN2) -> CN = SN1Op).

%SameOptions

numberOfElements( Number, Count, []):-
  Number = Count.
numberOfElements( Number, Count, [H|T] ):- 
  NewCount is Count + 1, numberOfElements( Number, NewCount, T).

sameOptions( SN1, SN2, Courses):-
  setof( Course, sharedCourse( SN1, SN2, Course), Courses),
  numberOfElements( 3, 0, Courses).
