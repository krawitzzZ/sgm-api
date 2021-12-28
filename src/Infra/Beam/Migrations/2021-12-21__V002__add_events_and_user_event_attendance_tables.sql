SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
CREATE TABLE "events" (
  "event_id" UUID NOT NULL,
  "created_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  "last_updated_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  "title" TEXT NOT NULL,
  "description" TEXT,
  "created_by" UUID NOT NULL,
  "last_updated_by" UUID NOT NULL,
  "start" TIMESTAMP NOT NULL,
  "end" TIMESTAMP NOT NULL,
  PRIMARY KEY("event_id")
);
CREATE TABLE "user_event_attendance_pivot" (
  "user_id" UUID NOT NULL,
  "event_id" UUID NOT NULL,
  "created_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  PRIMARY KEY("user_id", "event_id"),
  CONSTRAINT "user_event_attendance_pivot_user_id_fkey" FOREIGN KEY("user_id") REFERENCES "users"("user_id") ON DELETE CASCADE,
  CONSTRAINT "user_event_attendance_pivot_event_id_fkey" FOREIGN KEY("event_id") REFERENCES "events"("event_id") ON DELETE CASCADE
);
