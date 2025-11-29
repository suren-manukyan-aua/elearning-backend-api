> **Note:** This file has been generated using a large language model

# E-Learning Platform Backend API

A RESTful API for an E-Learning Platform, built with **Haskell** using the Servant web framework and PostgreSQL database.

## 📋 Table of Contents

- [Overview](#overview)
- [Tech Stack](#tech-stack)
- [Prerequisites](#prerequisites)
- [Quick Start](#quick-start)
- [API Endpoints](#api-endpoints)
- [Testing the API](#testing-the-api)
- [Project Structure](#project-structure)
- [Database Triggers](#database-triggers)

---

## Overview

This API serves as the backend for an E-Learning platform supporting:
- **Users**: Students, Instructors, and Administrators
- **Courses**: With sections, materials, and assignments
- **Assessments**: Homework submissions and quizzes
- **Communication**: Discussion forums with posts
- **Reporting**: Grade reports, GPA, enrollment statistics, and more

---

## Tech Stack

| Component | Technology |
|-----------|------------|
| Language | Haskell (GHC 9.x) |
| Web Framework | [Servant](https://docs.servant.dev/) — Type-safe REST API |
| Database | PostgreSQL 15 |
| DB Library | postgresql-simple |
| Build Tool | Stack |

### Why Haskell + Servant?

- **Type Safety**: API routes are checked at compile time — if it compiles, the routes are valid
- **No Runtime Route Errors**: Impossible to have mismatched URL parameters
- **Automatic JSON Handling**: Request/response serialization derived from types

---

## Prerequisites

1. **Haskell Stack**: [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
   ```bash
   curl -sSL https://get.haskellstack.org/ | sh
   ```

2. **PostgreSQL 15**: Either installed locally or via container
   ```bash
   # Using Podman/Docker
   podman run -d \
     --name elearning-db \
     -e POSTGRES_USER=postgres \
     -e POSTGRES_PASSWORD=postgres \
     -e POSTGRES_DB=elearning \
     -p 5432:5432 \
     postgres:15
   ```

---

## Quick Start

### 1. Initialize the Database

```bash
# Copy SQL files into the container
podman cp ddl.sql elearning-db:/ddl.sql
podman cp dql-init.sql elearning-db:/dql-init.sql
podman cp dml.sql elearning-db:/dml.sql

# Run the SQL files in order
podman exec -i elearning-db psql -U postgres -d elearning -f /ddl.sql
podman exec -i elearning-db psql -U postgres -d elearning -f /dql-init.sql
podman exec -i elearning-db psql -U postgres -d elearning -f /dml.sql

# Verify (should return 35)
podman exec -it elearning-db psql -U postgres -d elearning -c "SELECT COUNT(*) FROM student;"
```

### 2. Configure Database Connection

Edit `src/Db.hs` if your database credentials differ:

```haskell
createPool' :: IO DbPool
createPool' = newPool $ defaultPoolConfig
  (connect defaultConnectInfo
    { connectHost     = "localhost"
    , connectPort     = 5432
    , connectUser     = "postgres"      -- Change if needed
    , connectPassword = "postgres"      -- Change if needed
    , connectDatabase = "elearning"     -- Change if needed
    })
```

### 3. Build and Run

```bash
# Build the project (first build takes ~5-10 minutes)
stack build

# Run the server
stack exec backend-api-exe

# Server starts on http://localhost:8080
```

### 4. Test the API

```bash
# Get all students
curl http://localhost:8080/students

# Get a specific student
curl http://localhost:8080/students/1

# Get enrollment statistics
curl http://localhost:8080/reports/enrollment-stats
```

---

## API Endpoints

### Complete Endpoint Reference with Query Mapping

Each endpoint maps to a specific requirement from Phase 3 of the project.

#### 👤 User Management

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `GET` | `/students` | List all students | — |
| `GET` | `/students/:id` | Get student by ID | — |
| `POST` | `/students` | Register a new student | #1: Create user with role |
| `POST` | `/instructors` | Register a new instructor | #1: Create user with role |
| `POST` | `/administrators` | Register a new administrator | #1: Create user with role |

**Example — Create Student:**
```bash
curl -X POST http://localhost:8080/students \
  -H "Content-Type: application/json" \
  -d '{
    "email": "newstudent@example.com",
    "password": "securepass",
    "first_name": "John",
    "last_name": "Doe",
    "enrollment_date": "2025-01-15",
    "major": "Computer Science",
    "level": "Undergraduate"
  }'
```

---

#### 📚 Course Management

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `GET` | `/courses` | List all courses | — |
| `GET` | `/courses/:id` | Get course by ID | — |
| `POST` | `/courses` | Create a new course | #2: Add new course |
| `DELETE` | `/courses/:id` | Delete course (cascades) | #23: System operation |
| `POST` | `/teaches` | Assign instructor to section | #3: Assign instructor |

**Example — Create Course:**
```bash
curl -X POST http://localhost:8080/courses \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Machine Learning",
    "description": "Introduction to ML algorithms",
    "credits": 4
  }'
```

**Example — Assign Instructor:**
```bash
curl -X POST http://localhost:8080/teaches \
  -H "Content-Type: application/json" \
  -d '{
    "instructor_id": 36,
    "course_id": 1,
    "section_number": 1
  }'
```

---

#### 📝 Enrollment Management

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `POST` | `/enrollments` | Enroll student in section | #4: Student enroll |
| `GET` | `/enrollments/:studentId` | Get student's enrollments | — |
| `DELETE` | `/enrollments/:sid/:cid/:sec` | Unenroll (drop) student | #5: Student unenroll |
| `GET` | `/students/:id/courses` | View student's courses | #14: View my courses |

**Example — Enroll Student:**
```bash
curl -X POST http://localhost:8080/enrollments \
  -H "Content-Type: application/json" \
  -d '{
    "student_id": 5,
    "course_id": 1,
    "section_number": 1,
    "enrollment_date": "2025-03-01"
  }'
```

**Example — Unenroll Student:**
```bash
curl -X DELETE http://localhost:8080/enrollments/5/1/1
```

---

#### 📋 Assignments & Submissions

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `POST` | `/assignments` | Create homework assignment | #6: Create homework |
| `POST` | `/submissions` | Submit homework | #7: Submit homework |
| `GET` | `/submissions/assignment/:id` | View submissions for assignment | #10: View submissions |
| `PUT` | `/submissions/:id/grade` | Grade a submission | #10: Enter grade |

**Example — Create Assignment:**
```bash
curl -X POST http://localhost:8080/assignments \
  -H "Content-Type: application/json" \
  -d '{
    "course_id": 1,
    "section_number": 1,
    "title": "Homework 2: Recursion",
    "description": "Implement recursive functions",
    "due_date": "2025-04-01",
    "max_points": 100,
    "weight": 0.15
  }'
```

**Example — Grade Submission:**
```bash
curl -X PUT http://localhost:8080/submissions/1/grade \
  -H "Content-Type: application/json" \
  -d '{
    "grade": 95,
    "feedback": "Excellent work!"
  }'
```

---

#### 🧪 Quizzes

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `POST` | `/quizzes` | Create a quiz | #8: Create quiz |
| `POST` | `/quiz-attempts` | Submit quiz attempt | #9: Take quiz |
| `GET` | `/quiz-attempts/:quizId/:studentId` | View quiz attempts/scores | #9: View score |

**Example — Create Quiz:**
```bash
curl -X POST http://localhost:8080/quizzes \
  -H "Content-Type: application/json" \
  -d '{
    "course_id": 1,
    "section_number": 1,
    "title": "Midterm Quiz",
    "total_points": 50,
    "attempt_limit": 2
  }'
```

**Example — Submit Quiz Attempt:**
```bash
curl -X POST http://localhost:8080/quiz-attempts \
  -H "Content-Type: application/json" \
  -d '{
    "quiz_id": 1,
    "student_id": 1,
    "duration": 1800,
    "score": 45
  }'
```

---

#### 💬 Discussion Forums

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `POST` | `/posts` | Create forum post | #11: Create post |
| `GET` | `/posts/forum/:forumId` | Get posts in forum | — |

**Example — Create Post:**
```bash
curl -X POST http://localhost:8080/posts \
  -H "Content-Type: application/json" \
  -d '{
    "forum_id": 1,
    "user_id": 1,
    "content": "Can someone explain recursion?",
    "parent_post_id": null
  }'
```

> ⚠️ **Note:** The database trigger `enforce_post_membership` ensures:
> - Students can only post in forums for courses they're enrolled in
> - Instructors can only post in forums for courses they teach
> - Administrators can post anywhere

---

#### 📊 Reports

| Method | Endpoint | Description | Phase 3 Requirement |
|--------|----------|-------------|---------------------|
| `GET` | `/reports/grades/:studentId` | Student grade report | #12: View grade report |
| `GET` | `/reports/gpa/:studentId` | Student GPA | #13: View GPA |
| `GET` | `/reports/roster/:courseId/:section` | Class roster | #15: Generate roster |
| `GET` | `/reports/missing-submissions/:assignmentId` | Students who haven't submitted | #16: Missing submissions |
| `GET` | `/reports/enrollment-stats` | Course popularity ranking | #17: Enrollment statistics |
| `GET` | `/reports/quiz-stats/:quizId` | Quiz avg/high/low scores | #18: Quiz performance |
| `GET` | `/reports/at-risk/:courseId?threshold=60` | At-risk students | #19: Identify at-risk |
| `GET` | `/reports/instructor-workload` | Instructor course counts | #20: Workload report |
| `GET` | `/reports/forum-activity/:courseId` | Most active forum users | #21: Forum activity |
| `GET` | `/reports/platform-activity?start=DATE&end=DATE` | Platform-wide summary | #22: Activity summary |

**Example — Grade Report:**
```bash
curl http://localhost:8080/reports/grades/1
```

**Example — At-Risk Students (below 70%):**
```bash
curl "http://localhost:8080/reports/at-risk/1?threshold=70"
```

**Example — Platform Activity:**
```bash
curl "http://localhost:8080/reports/platform-activity?start=2025-01-01&end=2025-12-31"
```

---

## Testing the API

### Option 1: Swagger UI (Recommended)

Open `swagger-ui.html` in a browser, or use the online Swagger Editor:

1. Go to [https://editor.swagger.io](https://editor.swagger.io)
2. File → Import File → Select `openapi.yaml` from this project
3. Use the "Try it out" button on any endpoint

### Option 2: Run Test Script

```bash
chmod +x test-api.sh
./test-api.sh
```

### Option 3: Manual curl Commands

See examples above for each endpoint.

---

## Project Structure

```
backend-api/
├── app/
│   └── Main.hs              # Entry point — starts the server
├── src/
│   ├── Api.hs               # API route definitions (Servant)
│   ├── Db.hs                # Database queries
│   └── Types.hs             # Data types and JSON instances
├── openapi.yaml             # API specification
├── test/
│   └── test-api.sh          # Test script
├── package.yaml             # Dependencies
└── README.md                # This file
```

---

## Database Triggers

The database includes triggers that enforce business rules automatically:

| Trigger | Purpose |
|---------|---------|
| `constraint_check_user_subtype_*` | Each user must be exactly one of: Student, Instructor, or Administrator |
| `trg_enforce_post_membership` | Students can only post in forums for enrolled courses |
| `trg_enforce_submission_enrollment` | Students can only submit assignments for enrolled sections |
| `trg_enforce_quiz_score_max` | Quiz scores cannot exceed total points |
| `trg_enforce_attempt_limit` | Respects quiz attempt limits |
| `trg_enforce_grade_max` | Assignment grades cannot exceed max points |

These triggers fire automatically — no API code needed. If a rule is violated, the API returns an error response.

---

## Response Format

All responses follow this structure:

**Success:**
```json
{
  "success": true,
  "result": <data>,
  "errMsg": null
}
```

**Error:**
```json
{
  "success": false,
  "result": null,
  "errMsg": "Error description"
}
```
