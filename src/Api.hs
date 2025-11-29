{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( API,
    api,
    server,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (Day)
import Db
import Servant
import qualified Types as T

-- ============ API TYPE ============

type API =
  -- Students
  "students" :> Get '[JSON] [T.Student]
    :<|> "students" :> Capture "id" Int :> Get '[JSON] (T.ApiResponse T.Student)
    :<|> "students" :> ReqBody '[JSON] T.CreateStudent :> Post '[JSON] (T.ApiResponse Int)
    :<|> "students" :> Capture "id" Int :> "courses" :> Get '[JSON] [T.StudentCourse]
    -- Instructors
    :<|> "instructors" :> ReqBody '[JSON] T.CreateInstructor :> Post '[JSON] (T.ApiResponse Int)
    -- Administrators
    :<|> "administrators" :> ReqBody '[JSON] T.CreateAdmin :> Post '[JSON] (T.ApiResponse Int)
    -- Courses
    :<|> "courses" :> Get '[JSON] [T.Course]
    :<|> "courses" :> Capture "id" Int :> Get '[JSON] (T.ApiResponse T.Course)
    :<|> "courses" :> ReqBody '[JSON] T.CreateCourse :> Post '[JSON] (T.ApiResponse Int)
    :<|> "courses" :> Capture "id" Int :> Delete '[JSON] (T.ApiResponse Text)
    -- Assign Instructor
    :<|> "teaches" :> ReqBody '[JSON] T.AssignInstructor :> Post '[JSON] (T.ApiResponse Text)
    -- Enrollments
    :<|> "enrollments" :> ReqBody '[JSON] T.CreateEnrollment :> Post '[JSON] (T.ApiResponse Int)
    :<|> "enrollments" :> Capture "studentId" Int :> Get '[JSON] [T.Enrollment]
    :<|> "enrollments"
      :> Capture "studentId" Int
      :> Capture "courseId" Int
      :> Capture "section" Int
      :> Delete '[JSON] (T.ApiResponse Text)
    -- Assignments
    :<|> "assignments" :> ReqBody '[JSON] T.CreateAssignment :> Post '[JSON] (T.ApiResponse Int)
    -- Submissions
    :<|> "submissions" :> ReqBody '[JSON] T.CreateSubmission :> Post '[JSON] (T.ApiResponse Int)
    :<|> "submissions" :> "assignment" :> Capture "assignmentId" Int :> Get '[JSON] [T.Submission]
    :<|> "submissions"
      :> Capture "id" Int
      :> "grade"
      :> ReqBody '[JSON] T.GradeSubmission
      :> Put '[JSON] (T.ApiResponse Text)
    -- Quizzes
    :<|> "quizzes" :> ReqBody '[JSON] T.CreateQuiz :> Post '[JSON] (T.ApiResponse Int)
    -- Quiz Attempts
    :<|> "quiz-attempts" :> ReqBody '[JSON] T.CreateQuizAttempt :> Post '[JSON] (T.ApiResponse Int)
    :<|> "quiz-attempts"
      :> Capture "quizId" Int
      :> Capture "studentId" Int
      :> Get '[JSON] [T.QuizAttempt]
    -- Forum Posts
    :<|> "posts" :> ReqBody '[JSON] T.CreatePost :> Post '[JSON] (T.ApiResponse Int)
    :<|> "posts" :> "forum" :> Capture "forumId" Int :> Get '[JSON] [T.ForumPost]
    -- Reports
    :<|> "reports" :> "grades" :> Capture "studentId" Int :> Get '[JSON] [T.GradeReport]
    :<|> "reports" :> "gpa" :> Capture "studentId" Int :> Get '[JSON] (T.ApiResponse Double)
    :<|> "reports"
      :> "roster"
      :> Capture "courseId" Int
      :> Capture "section" Int
      :> Get '[JSON] [T.RosterEntry]
    :<|> "reports"
      :> "missing-submissions"
      :> Capture "assignmentId" Int
      :> Get '[JSON] [T.MissingSubmission]
    :<|> "reports" :> "enrollment-stats" :> Get '[JSON] [T.EnrollmentStat]
    :<|> "reports" :> "quiz-stats" :> Capture "quizId" Int :> Get '[JSON] (T.ApiResponse T.QuizStats)
    :<|> "reports"
      :> "at-risk"
      :> Capture "courseId" Int
      :> QueryParam "threshold" Double
      :> Get '[JSON] [T.AtRiskStudent]
    :<|> "reports" :> "instructor-workload" :> Get '[JSON] [T.InstructorWorkload]
    :<|> "reports" :> "forum-activity" :> Capture "courseId" Int :> Get '[JSON] [T.ForumActivity]
    :<|> "reports"
      :> "platform-activity"
      :> QueryParam "start" Day
      :> QueryParam "end" Day
      :> Get '[JSON] (T.ApiResponse T.PlatformActivity)

api :: Proxy API
api = Proxy

-- ============ SERVER ============

server :: DbPool -> Server API
server pool =
  getStudentsH
    :<|> getStudentH
    :<|> createStudentH
    :<|> getStudentCoursesH
    :<|> createInstructorH
    :<|> createAdminH
    :<|> getCoursesH
    :<|> getCourseH
    :<|> createCourseH
    :<|> deleteCourseH
    :<|> assignInstructorH
    :<|> enrollH
    :<|> getEnrollmentsH
    :<|> unenrollH
    :<|> createAssignmentH
    :<|> submitH
    :<|> getSubmissionsH
    :<|> gradeH
    :<|> createQuizH
    :<|> quizAttemptH
    :<|> getQuizAttemptsH
    :<|> postH
    :<|> getPostsH
    :<|> gradeReportH
    :<|> gpaH
    :<|> rosterH
    :<|> missingSubmissionsH
    :<|> enrollmentStatsH
    :<|> quizStatsH
    :<|> atRiskH
    :<|> workloadH
    :<|> forumActivityH
    :<|> platformActivityH
  where
    getStudentsH = liftIO $ withPool pool getAllStudents

    getStudentH sid = liftIO $ do
      res <- withPool pool (`getStudentById` sid)
      return $ case res of
        [s] -> T.okResponse s
        _ -> T.errResponse "Student not found"

    createStudentH cs = liftIO $ do
      res <- withPool pool (`createStudent` cs)
      return $ either T.errResponse T.okResponse res

    getStudentCoursesH sid = liftIO $ withPool pool (`getStudentCourses` sid)

    createInstructorH ci = liftIO $ do
      res <- withPool pool (`createInstructor` ci)
      return $ either T.errResponse T.okResponse res

    createAdminH ca = liftIO $ do
      res <- withPool pool (`createAdmin` ca)
      return $ either T.errResponse T.okResponse res

    getCoursesH = liftIO $ withPool pool getAllCourses

    getCourseH cid = liftIO $ do
      res <- withPool pool (`getCourseById` cid)
      return $ case res of
        [c] -> T.okResponse c
        _ -> T.errResponse "Course not found"

    createCourseH cc = liftIO $ do
      res <- withPool pool (`createCourse` cc)
      return $ either T.errResponse T.okResponse res

    deleteCourseH cid = liftIO $ do
      res <- withPool pool (`deleteCourse` cid)
      return $ case res of
        Right _ -> T.okResponse "Course deleted (cascade)"
        Left e -> T.errResponse e

    assignInstructorH ai = liftIO $ do
      res <- withPool pool (`assignInstructor` ai)
      return $ case res of
        Right _ -> T.okResponse "Instructor assigned"
        Left e -> T.errResponse e

    enrollH ce = liftIO $ do
      res <- withPool pool (`enrollStudent` ce)
      return $ either T.errResponse T.okResponse res

    getEnrollmentsH sid = liftIO $ withPool pool (`getEnrollmentsByStudent` sid)

    unenrollH sid cid sec = liftIO $ do
      res <- withPool pool (\c -> unenrollStudent c sid cid sec)
      return $ case res of
        Right _ -> T.okResponse "Unenrolled successfully"
        Left e -> T.errResponse e

    createAssignmentH ca = liftIO $ do
      res <- withPool pool (`createAssignment` ca)
      return $ either T.errResponse T.okResponse res

    submitH cs = liftIO $ do
      res <- withPool pool (`createSubmission` cs)
      return $ either T.errResponse T.okResponse res

    getSubmissionsH aid = liftIO $ withPool pool (`getSubmissionsByAssignment` aid)

    gradeH subId gs = liftIO $ do
      res <- withPool pool (\c -> gradeSubmission c subId gs)
      return $ case res of
        Right _ -> T.okResponse "Graded successfully"
        Left e -> T.errResponse e

    createQuizH cq = liftIO $ do
      res <- withPool pool (`createQuiz` cq)
      return $ either T.errResponse T.okResponse res

    quizAttemptH cqa = liftIO $ do
      res <- withPool pool (`createQuizAttempt` cqa)
      return $ either T.errResponse T.okResponse res

    getQuizAttemptsH qid sid = liftIO $ withPool pool (\c -> getQuizAttemptsByStudent c qid sid)

    postH cp = liftIO $ do
      res <- withPool pool (`createPost` cp)
      return $ either T.errResponse T.okResponse res

    getPostsH fid = liftIO $ withPool pool (`getPostsByForum` fid)

    gradeReportH sid = liftIO $ withPool pool (`getStudentGradeReport` sid)

    gpaH sid = liftIO $ do
      res <- withPool pool (`getStudentGPA` sid)
      return $ case res of
        [T.StudentGPA (Just g)] -> T.okResponse g
        _ -> T.errResponse "No grades found"

    rosterH cid sec = liftIO $ withPool pool (\c -> getClassRoster c cid sec)

    missingSubmissionsH aid = liftIO $ withPool pool (`getMissingSubmissions` aid)

    enrollmentStatsH = liftIO $ withPool pool getEnrollmentStats

    quizStatsH qid = liftIO $ do
      res <- withPool pool (`getQuizStats` qid)
      return $ case res of
        [qs] -> T.okResponse qs
        _ -> T.errResponse "Quiz not found"

    atRiskH cid mThresh =
      liftIO $
        withPool pool (\c -> getAtRiskStudents c cid (maybe 60.0 id mThresh))

    workloadH = liftIO $ withPool pool getInstructorWorkload

    forumActivityH cid = liftIO $ withPool pool (`getForumActivity` cid)

    platformActivityH mStart mEnd = liftIO $ do
      case (mStart, mEnd) of
        (Just s, Just e) -> do
          res <- withPool pool (\c -> getPlatformActivity c s e)
          return $ case res of
            [pa] -> T.okResponse pa
            _ -> T.errResponse "Query failed"
        _ -> return $ T.errResponse "Both start and end dates required"
