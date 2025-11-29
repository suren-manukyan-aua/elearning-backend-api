{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( -- * User
    User (..),
    CreateUser (..),

    -- * Student
    Student (..),
    CreateStudent (..),
    StudentCourse (..),

    -- * Instructor
    CreateInstructor (..),

    -- * Administrator
    CreateAdmin (..),

    -- * Course
    Course (..),
    CreateCourse (..),

    -- * Assignment
    Assignment (..),
    CreateAssignment (..),
    AssignInstructor (..),

    -- * Enrollment
    Enrollment (..),
    CreateEnrollment (..),

    -- * Submission
    Submission (..),
    CreateSubmission (..),
    GradeSubmission (..),

    -- * Quiz
    Quiz (..),
    CreateQuiz (..),
    QuizAttempt (..),
    CreateQuizAttempt (..),

    -- * Forum
    ForumPost (..),
    CreatePost (..),

    -- * Reports
    GradeReport (..),
    StudentGPA (..),
    RosterEntry (..),
    MissingSubmission (..),
    EnrollmentStat (..),
    QuizStats (..),
    AtRiskStudent (..),
    InstructorWorkload (..),
    ForumActivity (..),
    PlatformActivity (..),

    -- * Response
    ApiResponse (..),
    okResponse,
    errResponse,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    withObject,
    (.:),
    (.:?),
  )
import Data.Text (Text)
import Data.Time (Day, LocalTime)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import GHC.Generics (Generic)

-- ============ USER TYPES ============

data User = User
  { userId :: Int,
    userEmail :: Text,
    firstName :: Text,
    lastName :: Text,
    createdAt :: Maybe LocalTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field

data CreateUser = CreateUser
  { cuEmail :: Text,
    cuPassword :: Text,
    cuFirstName :: Text,
    cuLastName :: Text
  }
  deriving (Show, Generic)

instance FromJSON CreateUser where
  parseJSON = withObject "CreateUser" $ \v ->
    CreateUser
      <$> v .: "email"
      <*> v .: "password"
      <*> v .: "first_name"
      <*> v .: "last_name"

instance ToRow CreateUser where
  toRow u =
    [ toField (cuEmail u),
      toField (cuPassword u),
      toField (cuFirstName u),
      toField (cuLastName u)
    ]

-- ============ STUDENT TYPES ============

data Student = Student
  { studentId :: Int,
    studentEmail :: Text,
    studentFirstName :: Text,
    studentLastName :: Text,
    studentGpa :: Maybe Double,
    studentMajor :: Text,
    studentLevel :: Text,
    studentCredits :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow Student where
  fromRow =
    Student
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data CreateStudent = CreateStudent
  { csEmail :: Text,
    csPassword :: Text,
    csFirstName :: Text,
    csLastName :: Text,
    csEnrollmentDate :: Day,
    csMajor :: Text,
    csLevel :: Text
  }
  deriving (Show, Generic)

instance FromJSON CreateStudent where
  parseJSON = withObject "CreateStudent" $ \v ->
    CreateStudent
      <$> v .: "email"
      <*> v .: "password"
      <*> v .: "first_name"
      <*> v .: "last_name"
      <*> v .: "enrollment_date"
      <*> v .: "major"
      <*> v .: "level"

-- ============ COURSE TYPES ============

data Course = Course
  { courseId :: Int,
    courseTitle :: Text,
    courseDescription :: Maybe Text,
    courseCredits :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow Course where
  fromRow = Course <$> field <*> field <*> field <*> field

data CreateCourse = CreateCourse
  { ccTitle :: Text,
    ccDescription :: Maybe Text,
    ccCredits :: Int
  }
  deriving (Show, Generic)

instance FromJSON CreateCourse where
  parseJSON = withObject "CreateCourse" $ \v ->
    CreateCourse
      <$> v .: "title"
      <*> v .:? "description"
      <*> v .: "credits"

instance ToRow CreateCourse where
  toRow c = [toField (ccTitle c), toField (ccDescription c), toField (ccCredits c)]

-- ============ ENROLLMENT TYPES ============

data Enrollment = Enrollment
  { enrollmentId :: Int,
    enStudentId :: Int,
    enCourseId :: Int,
    enSectionNumber :: Int,
    enEnrollmentDate :: Day,
    enFinalGrade :: Maybe Double,
    enStatus :: Text
  }
  deriving (Show, Generic, ToJSON)

instance FromRow Enrollment where
  fromRow =
    Enrollment
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data CreateEnrollment = CreateEnrollment
  { ceStudentId :: Int,
    ceCourseId :: Int,
    ceSectionNumber :: Int,
    ceEnrollmentDate :: Day
  }
  deriving (Show, Generic)

instance FromJSON CreateEnrollment where
  parseJSON = withObject "CreateEnrollment" $ \v ->
    CreateEnrollment
      <$> v .: "student_id"
      <*> v .: "course_id"
      <*> v .: "section_number"
      <*> v .: "enrollment_date"

-- ============ SUBMISSION TYPES ============

data Submission = Submission
  { submissionId :: Int,
    subAssignmentId :: Int,
    subStudentId :: Int,
    submittedAt :: Maybe LocalTime,
    subGrade :: Maybe Double,
    subFeedback :: Maybe Text
  }
  deriving (Show, Generic, ToJSON)

instance FromRow Submission where
  fromRow = Submission <$> field <*> field <*> field <*> field <*> field <*> field

data CreateSubmission = CreateSubmission
  { csubAssignmentId :: Int,
    csubStudentId :: Int
  }
  deriving (Show, Generic)

instance FromJSON CreateSubmission where
  parseJSON = withObject "CreateSubmission" $ \v ->
    CreateSubmission
      <$> v .: "assignment_id"
      <*> v .: "student_id"

data GradeSubmission = GradeSubmission
  { gsGrade :: Double,
    gsFeedback :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON GradeSubmission where
  parseJSON = withObject "GradeSubmission" $ \v ->
    GradeSubmission
      <$> v .: "grade"
      <*> v .:? "feedback"

-- ============ QUIZ ATTEMPT TYPES ============

data QuizAttempt = QuizAttempt
  { attemptId :: Int,
    qaQuizId :: Int,
    qaStudentId :: Int,
    attemptTime :: Maybe LocalTime,
    qaScore :: Maybe Double,
    qaDuration :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow QuizAttempt where
  fromRow = QuizAttempt <$> field <*> field <*> field <*> field <*> field <*> field

data CreateQuizAttempt = CreateQuizAttempt
  { cqaQuizId :: Int,
    cqaStudentId :: Int,
    cqaDuration :: Int,
    cqaScore :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON CreateQuizAttempt where
  parseJSON = withObject "CreateQuizAttempt" $ \v ->
    CreateQuizAttempt
      <$> v .: "quiz_id"
      <*> v .: "student_id"
      <*> v .: "duration"
      <*> v .:? "score"

-- ============ POST (FORUM) TYPES ============

data ForumPost = ForumPost
  { postId :: Int,
    fpForumId :: Int,
    fpUserId :: Int,
    fpContent :: Text,
    postedAt :: Maybe LocalTime,
    parentPostId :: Maybe Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow ForumPost where
  fromRow = ForumPost <$> field <*> field <*> field <*> field <*> field <*> field

data CreatePost = CreatePost
  { cpForumId :: Int,
    cpUserId :: Int,
    cpContent :: Text,
    cpParentId :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON CreatePost where
  parseJSON = withObject "CreatePost" $ \v ->
    CreatePost
      <$> v .: "forum_id"
      <*> v .: "user_id"
      <*> v .: "content"
      <*> v .:? "parent_post_id"

-- ============ REPORT TYPES ============

data GradeReport = GradeReport
  { grCourseTitle :: Text,
    grAssignmentId :: Int,
    grAssignmentTitle :: Text,
    grGrade :: Maybe Double
  }
  deriving (Show, Generic, ToJSON)

instance FromRow GradeReport where
  fromRow = GradeReport <$> field <*> field <*> field <*> field

data StudentGPA = StudentGPA
  { sgpaValue :: Maybe Double
  }
  deriving (Show, Generic, ToJSON)

instance FromRow StudentGPA where
  fromRow = StudentGPA <$> field

data RosterEntry = RosterEntry
  { reUserId :: Int,
    reFirstName :: Text,
    reLastName :: Text
  }
  deriving (Show, Generic, ToJSON)

instance FromRow RosterEntry where
  fromRow = RosterEntry <$> field <*> field <*> field

data AtRiskStudent = AtRiskStudent
  { arsUserId :: Int,
    arsFirstName :: Text,
    arsLastName :: Text,
    arsAvgGrade :: Maybe Double
  }
  deriving (Show, Generic, ToJSON)

instance FromRow AtRiskStudent where
  fromRow = AtRiskStudent <$> field <*> field <*> field <*> field

data InstructorWorkload = InstructorWorkload
  { iwUserId :: Int,
    iwFirstName :: Text,
    iwLastName :: Text,
    iwCoursesTaught :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow InstructorWorkload where
  fromRow = InstructorWorkload <$> field <*> field <*> field <*> field

data ForumActivity = ForumActivity
  { faUserId :: Int,
    faFirstName :: Text,
    faLastName :: Text,
    faPostsCount :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow ForumActivity where
  fromRow = ForumActivity <$> field <*> field <*> field <*> field

data CreateInstructor = CreateInstructor
  { ciEmail :: Text,
    ciPassword :: Text,
    ciFirstName :: Text,
    ciLastName :: Text,
    ciDepartment :: Text,
    ciQualification :: Maybe Text,
    ciOffice :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON CreateInstructor where
  parseJSON = withObject "CreateInstructor" $ \v ->
    CreateInstructor
      <$> v .: "email"
      <*> v .: "password"
      <*> v .: "first_name"
      <*> v .: "last_name"
      <*> v .: "department"
      <*> v .:? "qualification"
      <*> v .:? "office"

-- ============ ADMINISTRATOR TYPES ============

data CreateAdmin = CreateAdmin
  { caEmail :: Text,
    caPassword :: Text,
    caFirstName :: Text,
    caLastName :: Text,
    caLevel :: Text -- "low", "mid", "high"
  }
  deriving (Show, Generic)

instance FromJSON CreateAdmin where
  parseJSON = withObject "CreateAdmin" $ \v ->
    CreateAdmin
      <$> v .: "email"
      <*> v .: "password"
      <*> v .: "first_name"
      <*> v .: "last_name"
      <*> v .: "level"

-- ============ TEACHES (ASSIGN INSTRUCTOR) ============

data AssignInstructor = AssignInstructor
  { aiInstructorId :: Int,
    aiCourseId :: Int,
    aiSectionNumber :: Int
  }
  deriving (Show, Generic)

instance FromJSON AssignInstructor where
  parseJSON = withObject "AssignInstructor" $ \v ->
    AssignInstructor
      <$> v .: "instructor_id"
      <*> v .: "course_id"
      <*> v .: "section_number"

-- ============ ASSIGNMENT TYPES ============

data Assignment = Assignment
  { assignmentId :: Int,
    asgCourseId :: Int,
    asgSectionNum :: Int,
    asgTitle :: Text,
    asgDescription :: Maybe Text,
    asgDueDate :: Maybe Day,
    asgMaxPoints :: Int,
    asgWeight :: Maybe Double
  }
  deriving (Show, Generic, ToJSON)

instance FromRow Assignment where
  fromRow =
    Assignment
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data CreateAssignment = CreateAssignment
  { casgnCourseId :: Int,
    casgnSectionNum :: Int,
    casgnTitle :: Text,
    casgnDescription :: Maybe Text,
    casgnDueDate :: Maybe Day,
    casgnMaxPoints :: Int,
    casgnWeight :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON CreateAssignment where
  parseJSON = withObject "CreateAssignment" $ \v ->
    CreateAssignment
      <$> v .: "course_id"
      <*> v .: "section_number"
      <*> v .: "title"
      <*> v .:? "description"
      <*> v .:? "due_date"
      <*> v .: "max_points"
      <*> v .:? "weight"

-- ============ QUIZ TYPES ============

data Quiz = Quiz
  { quizId :: Int,
    qzCourseId :: Int,
    qzSectionNum :: Int,
    qzTitle :: Text,
    qzTotalPoints :: Int,
    qzAttemptLimit :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow Quiz where
  fromRow = Quiz <$> field <*> field <*> field <*> field <*> field <*> field

data CreateQuiz = CreateQuiz
  { cqzCourseId :: Int,
    cqzSectionNum :: Int,
    cqzTitle :: Text,
    cqzTotalPoints :: Int,
    cqzAttemptLimit :: Int
  }
  deriving (Show, Generic)

instance FromJSON CreateQuiz where
  parseJSON = withObject "CreateQuiz" $ \v ->
    CreateQuiz
      <$> v .: "course_id"
      <*> v .: "section_number"
      <*> v .: "title"
      <*> v .: "total_points"
      <*> v .: "attempt_limit"

-- ============ STUDENT COURSES ============

data StudentCourse = StudentCourse
  { scCourseId :: Int,
    scTitle :: Text,
    scStatus :: Text
  }
  deriving (Show, Generic, ToJSON)

instance FromRow StudentCourse where
  fromRow = StudentCourse <$> field <*> field <*> field

-- ============ MISSING SUBMISSION REPORT ============

data MissingSubmission = MissingSubmission
  { msUserId :: Int,
    msFirstName :: Text,
    msLastName :: Text
  }
  deriving (Show, Generic, ToJSON)

instance FromRow MissingSubmission where
  fromRow = MissingSubmission <$> field <*> field <*> field

-- ============ ENROLLMENT STATS ============

data EnrollmentStat = EnrollmentStat
  { esCourseId :: Int,
    esTitle :: Text,
    esEnrollCount :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow EnrollmentStat where
  fromRow = EnrollmentStat <$> field <*> field <*> field

-- ============ QUIZ STATS ============

data QuizStats = QuizStats
  { qsAvgScore :: Maybe Double,
    qsHighScore :: Maybe Double,
    qsLowScore :: Maybe Double
  }
  deriving (Show, Generic, ToJSON)

instance FromRow QuizStats where
  fromRow = QuizStats <$> field <*> field <*> field

-- ============ PLATFORM ACTIVITY ============

data PlatformActivity = PlatformActivity
  { paNewEnrollments :: Int,
    paSubmissions :: Int,
    paPosts :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromRow PlatformActivity where
  fromRow = PlatformActivity <$> field <*> field <*> field

-- Simple response wrapper
data ApiResponse a = ApiResponse
  { success :: Bool,
    result :: Maybe a,
    errMsg :: Maybe Text
  }
  deriving (Show, Generic, ToJSON)

okResponse :: a -> ApiResponse a
okResponse x = ApiResponse True (Just x) Nothing

errResponse :: Text -> ApiResponse a
errResponse msg = ApiResponse False Nothing (Just msg)
