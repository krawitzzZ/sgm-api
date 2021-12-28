SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
CREATE TABLE "events" (
  "id" UUID NOT NULL,
  "created_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  "last_updated_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  "title" TEXT NOT NULL,
  "description" TEXT,
  "created_by" UUID NOT NULL,
  "last_updated_by" UUID NOT NULL,
  "start" TIMESTAMP NOT NULL,
  "end" TIMESTAMP NOT NULL,
  PRIMARY KEY("id")
);
CREATE TABLE "user_event_attendance_pivot" (
  "user_id" UUID NOT NULL,
  "event_id" UUID NOT NULL,
  "created_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  PRIMARY KEY("user_id", "event_id")
);
