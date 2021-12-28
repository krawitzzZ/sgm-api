SET client_encoding = 'UTF8';
CREATE TABLE "users" (
  "user_id" UUID NOT NULL,
  "created_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  "last_updated_at" TIMESTAMP DEFAULT NOW() NOT NULL,
  "username" TEXT NOT NULL UNIQUE,
  "password" TEXT NOT NULL,
  "roles" TEXT [9] NOT NULL,
  "first_name" TEXT,
  "last_name" TEXT,
  PRIMARY KEY("user_id")
);
CREATE EXTENSION "pgcrypto";
