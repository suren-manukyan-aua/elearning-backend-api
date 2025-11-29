{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Db
  ( -- * Connection Pool
    DbPool,
    createPool',
    withPool,

    -- * Users
    getAllUsers,
    getUserById,

    -- * Students
    createStudent,
    getAllStudents,
    getStudentById,
    getStudentCourses,

    -- * Instructors
    createInstructor,

    -- * Administrators
    createAdmin,

    -- * Courses
    getAllCourses,
    getCourseById,
    createCourse,
    deleteCourse,

    -- * Teaches
    assignInstructor,

    -- * Enrollments
    enrollStudent,
    unenrollStudent,
    getEnrollmentsByStudent,

    -- * Assignments
    createAssignment,

    -- * Submissions
    createSubmission,
    gradeSubmission,
    getSubmissionsByAssignment,

    -- * Quizzes
    createQuiz,

    -- * Quiz Attempts
    createQuizAttempt,
    getQuizAttemptsByStudent,

    -- * Posts
    createPost,
    getPostsByForum,

    -- * Reports
    getStudentGradeReport,
    getStudentGPA,
    getClassRoster,
    getMissingSubmissions,
    getEnrollmentStats,
    getQuizStats,
    getAtRiskStudents,
    getInstructorWorkload,
    getForumActivity,
    getPlatformActivity,
  )
where

import Control.Exception (SomeException, try)
import Data.Pool
import Data.Text (Text)
import Data.Time (Day)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Types as T

-- ============ CONNECTION ============

type DbPool = Pool Connection

createPool' :: IO DbPool
createPool' =
  newPool $
    defaultPoolConfig
      ( connect
          defaultConnectInfo
            { connectHost = "localhost",
              connectPort = 5432,
              connectUser = "postgres",
              connectPassword = "postgres",
              connectDatabase = "elearning"
            }
      )
      close
      60 -- cache TTL (idle time in seconds)
      20 -- max connections

withPool :: DbPool -> (Connection -> IO a) -> IO a
withPool = withResource

-- ============ USERS ============

getAllUsers :: Connection -> IO [T.User]
getAllUsers conn =
  query_
    conn
    [sql| SELECT user_id, email, first_name, last_name, created_at 
        FROM "user" ORDER BY user_id |]

getUserById :: Connection -> Int -> IO [T.User]
getUserById conn uid =
  query
    conn
    [sql| SELECT user_id, email, first_name, last_name, created_at 
        FROM "user" WHERE user_id = ? |]
    (Only uid)

-- ============ STUDENTS ============

createStudent :: Connection -> T.CreateStudent -> IO (Either Text Int)
createStudent conn cs = do
  res <- try $ withTransaction conn $ do
    [Only uid] <-
      query
        conn
        [sql| INSERT INTO "user" (email, password, first_name, last_name)
            VALUES (?, ?, ?, ?) RETURNING user_id |]
        (T.csEmail cs, T.csPassword cs, T.csFirstName cs, T.csLastName cs)
    _ <-
      execute
        conn
        [sql| INSERT INTO student (student_id, enrollment_date, major, level, gpa, credits)
            VALUES (?, ?, ?, ?, NULL, 0) |]
        (uid :: Int, T.csEnrollmentDate cs, T.csMajor cs, T.csLevel cs)
    return uid
  case res of
    Left e -> return $ Left (showError e)
    Right uid -> return $ Right uid

getAllStudents :: Connection -> IO [T.Student]
getAllStudents conn =
  query_
    conn
    [sql| SELECT s.student_id, u.email, u.first_name, u.last_name, 
               s.gpa::float8, s.major, s.level, s.credits
        FROM student s JOIN "user" u ON s.student_id = u.user_id
        ORDER BY s.student_id |]

getStudentById :: Connection -> Int -> IO [T.Student]
getStudentById conn sid =
  query
    conn
    [sql| SELECT s.student_id, u.email, u.first_name, u.last_name,
               s.gpa::float8, s.major, s.level, s.credits
        FROM student s JOIN "user" u ON s.student_id = u.user_id
        WHERE s.student_id = ? |]
    (Only sid)

getStudentCourses :: Connection -> Int -> IO [T.StudentCourse]
getStudentCourses conn sid =
  query
    conn
    [sql| SELECT c.course_id, c.title, e.status::text
        FROM enrollment e
        JOIN course c ON e.course_id = c.course_id
        WHERE e.student_id = ?
        ORDER BY c.title |]
    (Only sid)

-- ============ INSTRUCTORS ============

createInstructor :: Connection -> T.CreateInstructor -> IO (Either Text Int)
createInstructor conn ci = do
  res <- try $ withTransaction conn $ do
    [Only uid] <-
      query
        conn
        [sql| INSERT INTO "user" (email, password, first_name, last_name)
            VALUES (?, ?, ?, ?) RETURNING user_id |]
        (T.ciEmail ci, T.ciPassword ci, T.ciFirstName ci, T.ciLastName ci)
    _ <-
      execute
        conn
        [sql| INSERT INTO instructor (instructor_id, department, qualification, office)
            VALUES (?, ?, ?, ?) |]
        (uid :: Int, T.ciDepartment ci, T.ciQualification ci, T.ciOffice ci)
    return uid
  case res of
    Left e -> return $ Left (showError e)
    Right uid -> return $ Right uid

-- ============ ADMINISTRATORS ============

createAdmin :: Connection -> T.CreateAdmin -> IO (Either Text Int)
createAdmin conn ca = do
  res <- try $ withTransaction conn $ do
    [Only uid] <-
      query
        conn
        [sql| INSERT INTO "user" (email, password, first_name, last_name)
            VALUES (?, ?, ?, ?) RETURNING user_id |]
        (T.caEmail ca, T.caPassword ca, T.caFirstName ca, T.caLastName ca)
    _ <-
      execute
        conn
        [sql| INSERT INTO administrator (administrator_id, level)
            VALUES (?, ?::admin_level) |]
        (uid :: Int, T.caLevel ca)
    return uid
  case res of
    Left e -> return $ Left (showError e)
    Right uid -> return $ Right uid

-- ============ COURSES ============

getAllCourses :: Connection -> IO [T.Course]
getAllCourses conn =
  query_
    conn
    [sql| SELECT course_id, title, description, credits FROM course ORDER BY course_id |]

getCourseById :: Connection -> Int -> IO [T.Course]
getCourseById conn cid =
  query
    conn
    [sql| SELECT course_id, title, description, credits FROM course WHERE course_id = ? |]
    (Only cid)

createCourse :: Connection -> T.CreateCourse -> IO (Either Text Int)
createCourse conn cc = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO course (title, description, credits)
          VALUES (?, ?, ?) RETURNING course_id |]
        (T.ccTitle cc, T.ccDescription cc, T.ccCredits cc)
  case res of
    Left e -> return $ Left (showError e)
    Right [Only cid] -> return $ Right cid
    Right _ -> return $ Left "Unexpected result from insert"

deleteCourse :: Connection -> Int -> IO (Either Text ())
deleteCourse conn cid = do
  res <-
    try $
      execute
        conn
        [sql| DELETE FROM course WHERE course_id = ? |]
        (Only cid)
  case res of
    Left e -> return $ Left (showError e)
    Right 0 -> return $ Left "Course not found"
    Right _ -> return $ Right ()

-- ============ ASSIGN INSTRUCTOR ============

assignInstructor :: Connection -> T.AssignInstructor -> IO (Either Text ())
assignInstructor conn ai = do
  res <-
    try $
      execute
        conn
        [sql| INSERT INTO teaches (instructor_id, course_id, section_number)
          VALUES (?, ?, ?) |]
        (T.aiInstructorId ai, T.aiCourseId ai, T.aiSectionNumber ai)
  case res of
    Left e -> return $ Left (showError e)
    Right _ -> return $ Right ()

-- ============ ENROLLMENTS ============

enrollStudent :: Connection -> T.CreateEnrollment -> IO (Either Text Int)
enrollStudent conn ce = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO enrollment (student_id, course_id, section_number, enrollment_date, status)
          VALUES (?, ?, ?, ?, 'enrolled') RETURNING enrollment_id |]
        (T.ceStudentId ce, T.ceCourseId ce, T.ceSectionNumber ce, T.ceEnrollmentDate ce)
  case res of
    Left e -> return $ Left (showError e)
    Right [Only eid] -> return $ Right eid
    Right _ -> return $ Left "Unexpected result"

unenrollStudent :: Connection -> Int -> Int -> Int -> IO (Either Text ())
unenrollStudent conn sid cid secNum = do
  res <-
    try $
      execute
        conn
        [sql| UPDATE enrollment SET status = 'dropped', final_grade = NULL
          WHERE student_id = ? AND course_id = ? AND section_number = ? |]
        (sid, cid, secNum)
  case res of
    Left e -> return $ Left (showError e)
    Right 0 -> return $ Left "Enrollment not found"
    Right _ -> return $ Right ()

getEnrollmentsByStudent :: Connection -> Int -> IO [T.Enrollment]
getEnrollmentsByStudent conn sid =
  query
    conn
    [sql| SELECT enrollment_id, student_id, course_id, section_number, 
               enrollment_date, final_grade::float8, status::text
        FROM enrollment WHERE student_id = ? |]
    (Only sid)

-- ============ ASSIGNMENTS ============

createAssignment :: Connection -> T.CreateAssignment -> IO (Either Text Int)
createAssignment conn ca = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO assignment (course_id, section_number, title, description, due_date, max_points, weight)
          VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING assignment_id |]
        ( T.casgnCourseId ca,
          T.casgnSectionNum ca,
          T.casgnTitle ca,
          T.casgnDescription ca,
          T.casgnDueDate ca,
          T.casgnMaxPoints ca,
          T.casgnWeight ca
        )
  case res of
    Left e -> return $ Left (showError e)
    Right [Only aid] -> return $ Right aid
    Right _ -> return $ Left "Unexpected result"

-- ============ SUBMISSIONS ============

createSubmission :: Connection -> T.CreateSubmission -> IO (Either Text Int)
createSubmission conn cs = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO submission (assignment_id, student_id)
          VALUES (?, ?) RETURNING submission_id |]
        (T.csubAssignmentId cs, T.csubStudentId cs)
  case res of
    Left e -> return $ Left (showError e)
    Right [Only sid] -> return $ Right sid
    Right _ -> return $ Left "Unexpected result"

gradeSubmission :: Connection -> Int -> T.GradeSubmission -> IO (Either Text ())
gradeSubmission conn subId gs = do
  res <-
    try $
      execute
        conn
        [sql| UPDATE submission SET grade = ?, feedback = ? WHERE submission_id = ? |]
        (T.gsGrade gs, T.gsFeedback gs, subId)
  case res of
    Left e -> return $ Left (showError e)
    Right 0 -> return $ Left "Submission not found"
    Right _ -> return $ Right ()

getSubmissionsByAssignment :: Connection -> Int -> IO [T.Submission]
getSubmissionsByAssignment conn aid =
  query
    conn
    [sql| SELECT submission_id, assignment_id, student_id, submitted_at, grade::float8, feedback
        FROM submission WHERE assignment_id = ? |]
    (Only aid)

-- ============ QUIZZES ============

createQuiz :: Connection -> T.CreateQuiz -> IO (Either Text Int)
createQuiz conn cq = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO quiz (course_id, section_number, title, total_points, attempt_limit)
          VALUES (?, ?, ?, ?, ?) RETURNING quiz_id |]
        (T.cqzCourseId cq, T.cqzSectionNum cq, T.cqzTitle cq, T.cqzTotalPoints cq, T.cqzAttemptLimit cq)
  case res of
    Left e -> return $ Left (showError e)
    Right [Only qid] -> return $ Right qid
    Right _ -> return $ Left "Unexpected result"

-- ============ QUIZ ATTEMPTS ============

createQuizAttempt :: Connection -> T.CreateQuizAttempt -> IO (Either Text Int)
createQuizAttempt conn cqa = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO quiz_attempt (quiz_id, student_id, duration, score)
          VALUES (?, ?, ?, ?) RETURNING attempt_id |]
        (T.cqaQuizId cqa, T.cqaStudentId cqa, T.cqaDuration cqa, T.cqaScore cqa)
  case res of
    Left e -> return $ Left (showError e)
    Right [Only aid] -> return $ Right aid
    Right _ -> return $ Left "Unexpected result"

getQuizAttemptsByStudent :: Connection -> Int -> Int -> IO [T.QuizAttempt]
getQuizAttemptsByStudent conn qid sid =
  query
    conn
    [sql| SELECT attempt_id, quiz_id, student_id, attempt_time, score::float8, duration
        FROM quiz_attempt WHERE quiz_id = ? AND student_id = ?
        ORDER BY attempt_time DESC |]
    (qid, sid)

-- ============ FORUM POSTS ============

createPost :: Connection -> T.CreatePost -> IO (Either Text Int)
createPost conn cp = do
  res <-
    try $
      query
        conn
        [sql| INSERT INTO post (forum_id, user_id, content, parent_post_id)
          VALUES (?, ?, ?, ?) RETURNING post_id |]
        (T.cpForumId cp, T.cpUserId cp, T.cpContent cp, T.cpParentId cp)
  case res of
    Left e -> return $ Left (showError e)
    Right [Only pid] -> return $ Right pid
    Right _ -> return $ Left "Unexpected result"

getPostsByForum :: Connection -> Int -> IO [T.ForumPost]
getPostsByForum conn fid =
  query
    conn
    [sql| SELECT post_id, forum_id, user_id, content, posted_at, parent_post_id
        FROM post WHERE forum_id = ? ORDER BY posted_at |]
    (Only fid)

-- ============ REPORTS ============

getStudentGradeReport :: Connection -> Int -> IO [T.GradeReport]
getStudentGradeReport conn sid =
  query
    conn
    [sql| SELECT c.title, a.assignment_id, a.title, s.grade::float8
        FROM enrollment e
        JOIN course c ON e.course_id = c.course_id
        JOIN assignment a ON a.course_id = c.course_id AND a.section_number = e.section_number
        LEFT JOIN submission s ON s.assignment_id = a.assignment_id AND s.student_id = e.student_id
        WHERE e.student_id = ?
        ORDER BY c.title, a.assignment_id |]
    (Only sid)

getStudentGPA :: Connection -> Int -> IO [T.StudentGPA]
getStudentGPA conn sid =
  query
    conn
    [sql| SELECT (AVG(s.grade) / 20.0)::float8
        FROM submission s WHERE s.student_id = ? |]
    (Only sid)

getClassRoster :: Connection -> Int -> Int -> IO [T.RosterEntry]
getClassRoster conn cid secNum =
  query
    conn
    [sql| SELECT u.user_id, u.first_name, u.last_name
        FROM enrollment e
        JOIN "user" u ON e.student_id = u.user_id
        WHERE e.course_id = ? AND e.section_number = ? |]
    (cid, secNum)

getMissingSubmissions :: Connection -> Int -> IO [T.MissingSubmission]
getMissingSubmissions conn asgId =
  query
    conn
    [sql| SELECT u.user_id, u.first_name, u.last_name
        FROM enrollment e
        JOIN "user" u ON u.user_id = e.student_id
        JOIN assignment a ON a.course_id = e.course_id AND a.section_number = e.section_number
        LEFT JOIN submission s ON s.assignment_id = a.assignment_id AND s.student_id = e.student_id
        WHERE a.assignment_id = ?
          AND s.submission_id IS NULL |]
    (Only asgId)

getEnrollmentStats :: Connection -> IO [T.EnrollmentStat]
getEnrollmentStats conn =
  query_
    conn
    [sql| SELECT c.course_id, c.title, COUNT(e.student_id)::int AS enrollment_count
        FROM course c
        LEFT JOIN enrollment e ON c.course_id = e.course_id
        GROUP BY c.course_id, c.title
        ORDER BY enrollment_count DESC |]

getQuizStats :: Connection -> Int -> IO [T.QuizStats]
getQuizStats conn qid =
  query
    conn
    [sql| SELECT AVG(score)::float8, MAX(score)::float8, MIN(score)::float8
        FROM quiz_attempt
        WHERE quiz_id = ? |]
    (Only qid)

getAtRiskStudents :: Connection -> Int -> Double -> IO [T.AtRiskStudent]
getAtRiskStudents conn cid threshold =
  query
    conn
    [sql| SELECT u.user_id, u.first_name, u.last_name, AVG(s.grade)::float8
        FROM enrollment e
        JOIN "user" u ON e.student_id = u.user_id
        JOIN assignment a ON a.course_id = e.course_id AND a.section_number = e.section_number
        LEFT JOIN submission s ON s.assignment_id = a.assignment_id AND s.student_id = u.user_id
        WHERE e.course_id = ?
        GROUP BY u.user_id, u.first_name, u.last_name
        HAVING AVG(s.grade) < ? OR AVG(s.grade) IS NULL |]
    (cid, threshold)

getInstructorWorkload :: Connection -> IO [T.InstructorWorkload]
getInstructorWorkload conn =
  query_
    conn
    [sql| SELECT u.user_id, u.first_name, u.last_name, COUNT(t.course_id)::int
        FROM "user" u
        JOIN teaches t ON u.user_id = t.instructor_id
        GROUP BY u.user_id, u.first_name, u.last_name
        ORDER BY COUNT(t.course_id) DESC |]

getForumActivity :: Connection -> Int -> IO [T.ForumActivity]
getForumActivity conn cid =
  query
    conn
    [sql| SELECT u.user_id, u.first_name, u.last_name, COUNT(p.post_id)::int
        FROM post p
        JOIN "user" u ON p.user_id = u.user_id
        JOIN forum f ON p.forum_id = f.forum_id
        WHERE f.course_id = ?
        GROUP BY u.user_id, u.first_name, u.last_name
        ORDER BY COUNT(p.post_id) DESC |]
    (Only cid)

getPlatformActivity :: Connection -> Day -> Day -> IO [T.PlatformActivity]
getPlatformActivity conn startDate endDate =
  query
    conn
    [sql| SELECT
          (SELECT COUNT(*)::int FROM enrollment 
           WHERE enrollment_date BETWEEN ? AND ?),
          (SELECT COUNT(*)::int FROM submission 
           WHERE submitted_at BETWEEN ? AND ?),
          (SELECT COUNT(*)::int FROM post 
           WHERE posted_at BETWEEN ? AND ?) |]
    (startDate, endDate, startDate, endDate, startDate, endDate)

-- ============ HELPER ============

showError :: SomeException -> Text
showError e = "Database error: " <> (read . show . show $ e)
