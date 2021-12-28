# Things to do

## General

* check if LocalTime validation (Validity) works correctly
* refresh token on every request cuz access token is short living?

## Auth

* handle roles manadgement (add, revoke, adjust, etc)
* return 404 if user not found on login

## User

* add user image
* password recovery
* get all users filtered by name

## Event

* check if pivot table needs to be updated on event/user removal or if this can be done via ON DELETE CASCADE in migration
* unattend an event
* add query params to `allEvents` -> get only those in the future, with sorting, title or description including, etc
* add location to events

## Infra

* check that foreign key constraints work
* wrap migration execution in transaction as suggested in postgres readme
