
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



%%% Question 3 %%%

% enrolled predicate: Predicate answers relation of which student is taking
% which course. 
enrolled( SID, SCN ):-
  student( SID, _, _),             % Check individual is student
  ( compCourse( SCN, _, _);        % Each student takes all compulsory courses
    enrolled_opt( SID, SCN ) ).    % Find optional courses for student



%%% Question 4 %%%
                             
% teaches predicate: Predicate answers relation of which teacher teaches
% which course. 
teaches( TN, SCN):-
  teacher( TID, TN),               % Check individual is teacher and get ID
  ( compCourse( SCN, _, TID);      % Get compulsory SCN ...
    optCourse( SCN, _, TID) ).     % or optional SCN that teacher teaches



%%% Question 5 %%%

% taughtBy predicate: Predicate answers relation of which student is taught
% by which teachers. Query must be student full name, and teacher full name.
taughtBy( SN, TN):-
  student( SID, SN, _),            % Check individual is student and get ID
  enrolled( SID, SCN),             % Get courses they are enrolled in
  teaches( TN, SCN).               % Get TN of teaches that teach those courses



%%% Question 6 %%%

% takesOption predicate: Predicate answers relation of which student
% takes which optional course.
takesOption( SN, CN):-
  student( SID, SN, _),            % Check individual is student and get ID
  enrolled_opt( SID, SCN ),        % Get SCN for an option SN takes
  optCourse( SCN, CN, _).          % Get full course name for option



%%% Question 7 %%%

% takesAllOptions predicate: Predicate relates all the courses a student takes
% to the name of the student.
takesAllOptions( SN, OptCourses) :-
  student( _, SN, _),              % Check individual is student
  setof( CN, takesOption( SN, CN), OptCourses). % Generate list of options



%%% Question 8 %%%

% studentsInHouse predicate: Predicate generates a list of all students
% in house H.
inhouse(SID, H):-
  student(SID, _, H).

% snFromSID: Generates list of student names from list of student ID's
snFromSID( [], []).                       % Base case
snFromSID( [SID_H|SID_T], [SN_H|SN_T]):-  % Take SID list and SN list
  student( SID_H, SN, _),                 % Find SN corresponding to SID element
  SN_H = SN,                              % Unify SN list element to SN
  snFromSID( SID_T, SN_T).                % Recurse to next element

studentsInHouse(H, Students):-
  setof(SID, inhouse(SID, H), SIDList),   % Generate SID list
  snFromSID( SIDList, Students).          % Convert to SN list



%%% Question 9 %%% 

% studentsOnCourse: Predicate generates a key-value lisitng of all the houses
% (keys) and all the students (values) that are on a course

studentsOnCourse( SCN, CN, StudentsByHouse ):-
  ( compCourse( SCN, CN, _ ) ; optCourse( SCN, CN, _) ), % convert CN to SCN
  findall( House-Students,
         ( houses( HouseSet),           % Get list of all houses
           member( House, HouseSet),    % Set each House as key
           houseStudents( House, SCN, Students) ),  % get students from House on course SCN 
           StudentsByHouse).

houseStudents( House, SCN, Students ):- % Gen list of all students from a House on a course
  findall( SN, 
         ( courseStudents_SN( SCN, CourseStudents), % List all students on course
           member( SN, CourseStudents),   % Get each student from course...
           checkHouse( SN, House) ),      % and check if their house matches House
           Students).    % list of students on SCN that are in House


courseStudents_SID( SCN, CourseStudents ):- % ID list of students on a course
  bagof( Student, enrolled( Student, SCN), CourseStudents).

courseStudents_SN( SCN, CourseStudents ):-  %Full name list of students on a course
  courseStudents_SID( SCN, SIDList),
  snFromSID( SIDList, CourseStudents).

checkHouse( SN, H ):-                     % Check if a student SN is in house H
  student(_, SN, H).

houses( Houses ):-                        % Generate list of houses 
  setof( House, house(House), Houses).



%%% Question 10 %%%

%sharedCourse: Generates optional courses shared by two students
sharedCourse( SN1, SN2, CN ):-
  takesAllOptions( SN1, SN1Opts),  % Generate list of optional courses for SN1
  member( SN1Op, SN1Opts),         % Get each optional course...
  (student( SID2, SN2, _),         
  optCourse( SCN1, SN1Op, _),      % get the SCN for the optional course
  enrolled_opt( SID2, SCN1) -> CN = SN1Op). % if SN2 is enrolled in a course
                                            % of SN1 then unify course to answer


%%% Question 11 %%%

%sameOptions: generates a list of options shared by two students if they
% share exactly three.
numberOfElements( Number, Count, []):-
  Number = Count.   % if number of elements is not equal to set number, fail

numberOfElements( Number, Count, [H|T] ):-  % count elements in list
  NewCount is Count + 1, numberOfElements( Number, NewCount, T).

sameOptions( SN1, SN2, Courses):-
  setof( Course, sharedCourse( SN1, SN2, Course), Courses), % get list of shared courses
  numberOfElements( 3, 0, Courses).         % check if list length is 3
