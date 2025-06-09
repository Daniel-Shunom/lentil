import gleam/dynamic/decode
import gleam/option.{type Option}
import pog

/// Runs the `update_room_data` query
/// defined in `./src/lntl_server/sql/update_room_data.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_room_data(db, arg_1, arg_2, arg_3, arg_4) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "UPDATE lntl.rooms
SET
  name       = $1,
  capacity   = $2,
  status     = $3,
  updated_at = now()
WHERE id = $4
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.int(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `update_message_status` query
/// defined in `./src/lntl_server/sql/update_message_status.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_message_status(db, arg_1, arg_2) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "UPDATE lntl.messages
SET status = $1
WHERE id = $2
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `update_message_as_edited` query
/// defined in `./src/lntl_server/sql/update_message_as_edited.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_message_as_edited(db, arg_1, arg_2) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "UPDATE lntl.messages
SET
  content   = $1,
  edited_at = now()
WHERE id = $2
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_user_session` query
/// defined in `./src/lntl_server/sql/fetch_user_session.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchUserSessionRow {
  FetchUserSessionRow(
    session_id: String,
    user_id: Option(String),
    created_at: String,
    expires_at: String,
  )
}

/// Runs the `fetch_user_session` query
/// defined in `./src/lntl_server/sql/fetch_user_session.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_user_session(db, arg_1) {
  let decoder = {
    use session_id <- decode.field(0, decode.string)
    use user_id <- decode.field(1, decode.optional(decode.string))
    use created_at <- decode.field(2, decode.string)
    use expires_at <- decode.field(3, decode.string)
    decode.success(
      FetchUserSessionRow(session_id:, user_id:, created_at:, expires_at:),
    )
  }

  "SELECT
  session_id,
  user_id,
  created_at::TEXT   AS created_at,
  expires_at::TEXT   AS expires_at
FROM lntl.sessions
WHERE session_id = $1
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_user_room_memberships` query
/// defined in `./src/lntl_server/sql/fetch_user_room_memberships.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchUserRoomMembershipsRow {
  FetchUserRoomMembershipsRow(room_id: String, joined_at: String)
}

/// name: fetch_user_room_memberships :many
/// Return the IDs of all rooms a user belongs to, along with when they joined
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_user_room_memberships(db, arg_1) {
  let decoder = {
    use room_id <- decode.field(0, decode.string)
    use joined_at <- decode.field(1, decode.string)
    decode.success(FetchUserRoomMembershipsRow(room_id:, joined_at:))
  }

  "-- name: fetch_user_room_memberships :many
-- Return the IDs of all rooms a user belongs to, along with when they joined
SELECT
  rm.room_id,
  rm.joined_at::TEXT   AS joined_at
FROM lntl.room_members AS rm
WHERE rm.user_id = $1
ORDER BY rm.joined_at ASC
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_user_by_id` query
/// defined in `./src/lntl_server/sql/fetch_user_by_id.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchUserByIdRow {
  FetchUserByIdRow(
    id: String,
    first_name: String,
    last_name: String,
    username: String,
    dob_day: Int,
    dob_month: Int,
    dob_year: Int,
    gender: String,
    pronouns: String,
    is_authenticated: Option(Bool),
    created_at: String,
  )
}

/// Runs the `fetch_user_by_id` query
/// defined in `./src/lntl_server/sql/fetch_user_by_id.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_user_by_id(db, arg_1) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    use first_name <- decode.field(1, decode.string)
    use last_name <- decode.field(2, decode.string)
    use username <- decode.field(3, decode.string)
    use dob_day <- decode.field(4, decode.int)
    use dob_month <- decode.field(5, decode.int)
    use dob_year <- decode.field(6, decode.int)
    use gender <- decode.field(7, decode.string)
    use pronouns <- decode.field(8, decode.string)
    use is_authenticated <- decode.field(9, decode.optional(decode.bool))
    use created_at <- decode.field(10, decode.string)
    decode.success(
      FetchUserByIdRow(
        id:,
        first_name:,
        last_name:,
        username:,
        dob_day:,
        dob_month:,
        dob_year:,
        gender:,
        pronouns:,
        is_authenticated:,
        created_at:,
      ),
    )
  }

  "SELECT
  id,
  first_name,
  last_name,
  username,
  dob_day,
  dob_month,
  dob_year,
  gender,
  pronouns,
  is_authenticated,
  created_at::TEXT   AS created_at
FROM lntl.users
WHERE id = $1
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_user` query
/// defined in `./src/lntl_server/sql/fetch_user.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchUserRow {
  FetchUserRow(
    id: String,
    first_name: String,
    last_name: String,
    username: String,
    dob_day: Int,
    dob_month: Int,
    dob_year: Int,
    gender: String,
    pronouns: String,
  )
}

/// Runs the `fetch_user` query
/// defined in `./src/lntl_server/sql/fetch_user.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_user(db, arg_1, arg_2) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    use first_name <- decode.field(1, decode.string)
    use last_name <- decode.field(2, decode.string)
    use username <- decode.field(3, decode.string)
    use dob_day <- decode.field(4, decode.int)
    use dob_month <- decode.field(5, decode.int)
    use dob_year <- decode.field(6, decode.int)
    use gender <- decode.field(7, decode.string)
    use pronouns <- decode.field(8, decode.string)
    decode.success(
      FetchUserRow(
        id:,
        first_name:,
        last_name:,
        username:,
        dob_day:,
        dob_month:,
        dob_year:,
        gender:,
        pronouns:,
      ),
    )
  }

  "SELECT
  id,
  first_name,
  last_name,
  username,
  dob_day,
  dob_month,
  dob_year,
  gender,
  pronouns
FROM lntl.users
WHERE username      = $1
  AND password_hash = $2
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_room_messages` query
/// defined in `./src/lntl_server/sql/fetch_room_messages.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchRoomMessagesRow {
  FetchRoomMessagesRow(
    id: String,
    room_id: Option(String),
    user_id: Option(String),
    content: String,
    timestamp: String,
    status: String,
    edited_at: String,
    deleted_at: String,
  )
}

/// Runs the `fetch_room_messages` query
/// defined in `./src/lntl_server/sql/fetch_room_messages.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_room_messages(db, arg_1, arg_2, arg_3) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    use room_id <- decode.field(1, decode.optional(decode.string))
    use user_id <- decode.field(2, decode.optional(decode.string))
    use content <- decode.field(3, decode.string)
    use timestamp <- decode.field(4, decode.string)
    use status <- decode.field(5, decode.string)
    use edited_at <- decode.field(6, decode.string)
    use deleted_at <- decode.field(7, decode.string)
    decode.success(
      FetchRoomMessagesRow(
        id:,
        room_id:,
        user_id:,
        content:,
        timestamp:,
        status:,
        edited_at:,
        deleted_at:,
      ),
    )
  }

  "SELECT
  id,
  room_id,
  user_id,
  content,
  timestamp::TEXT      AS timestamp,
  status,
  edited_at::TEXT      AS edited_at,
  deleted_at::TEXT     AS deleted_at
FROM lntl.messages
WHERE room_id = $1
ORDER BY timestamp ASC
LIMIT  $2
OFFSET $3
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.int(arg_2))
  |> pog.parameter(pog.int(arg_3))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_room_members` query
/// defined in `./src/lntl_server/sql/fetch_room_members.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchRoomMembersRow {
  FetchRoomMembersRow(
    id: String,
    first_name: String,
    last_name: String,
    username: String,
    dob_day: Int,
    dob_month: Int,
    dob_year: Int,
    gender: String,
    pronouns: String,
  )
}

/// Runs the `fetch_room_members` query
/// defined in `./src/lntl_server/sql/fetch_room_members.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_room_members(db, arg_1) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    use first_name <- decode.field(1, decode.string)
    use last_name <- decode.field(2, decode.string)
    use username <- decode.field(3, decode.string)
    use dob_day <- decode.field(4, decode.int)
    use dob_month <- decode.field(5, decode.int)
    use dob_year <- decode.field(6, decode.int)
    use gender <- decode.field(7, decode.string)
    use pronouns <- decode.field(8, decode.string)
    decode.success(
      FetchRoomMembersRow(
        id:,
        first_name:,
        last_name:,
        username:,
        dob_day:,
        dob_month:,
        dob_year:,
        gender:,
        pronouns:,
      ),
    )
  }

  "SELECT
  u.id,
  u.first_name,
  u.last_name,
  u.username,
  u.dob_day,
  u.dob_month,
  u.dob_year,
  u.gender,
  u.pronouns
FROM lntl.users AS u
JOIN lntl.room_members AS rm
  ON u.id = rm.user_id
WHERE rm.room_id = $1
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `fetch_room_by_id` query
/// defined in `./src/lntl_server/sql/fetch_room_by_id.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FetchRoomByIdRow {
  FetchRoomByIdRow(
    id: String,
    owner_id: Option(String),
    name: String,
    capacity: Int,
    status: String,
    created_at: String,
    updated_at: String,
  )
}

/// Runs the `fetch_room_by_id` query
/// defined in `./src/lntl_server/sql/fetch_room_by_id.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn fetch_room_by_id(db, arg_1) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    use owner_id <- decode.field(1, decode.optional(decode.string))
    use name <- decode.field(2, decode.string)
    use capacity <- decode.field(3, decode.int)
    use status <- decode.field(4, decode.string)
    use created_at <- decode.field(5, decode.string)
    use updated_at <- decode.field(6, decode.string)
    decode.success(
      FetchRoomByIdRow(
        id:,
        owner_id:,
        name:,
        capacity:,
        status:,
        created_at:,
        updated_at:,
      ),
    )
  }

  "SELECT
  id,
  owner_id,
  name,
  capacity,
  status,
  created_at::TEXT   AS created_at,
  updated_at::TEXT   AS updated_at
FROM lntl.rooms
WHERE id = $1
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `delete_user_session` query
/// defined in `./src/lntl_server/sql/delete_user_session.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn delete_user_session(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "DELETE FROM lntl.sessions
WHERE session_id = $1
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `delete_user_from_room` query
/// defined in `./src/lntl_server/sql/delete_user_from_room.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn delete_user_from_room(db, arg_1, arg_2) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "DELETE FROM lntl.room_members
WHERE room_id = $1
  AND user_id = $2
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `delete_room` query
/// defined in `./src/lntl_server/sql/delete_room.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn delete_room(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "DELETE FROM lntl.rooms
WHERE id = $1
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `delete_message_temp` query
/// defined in `./src/lntl_server/sql/delete_message_temp.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn delete_message_temp(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "UPDATE lntl.messages
SET deleted_at = now(),
    status     = 'DELETED'
WHERE id = $1
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `delete_all_user_sessions` query
/// defined in `./src/lntl_server/sql/delete_all_user_sessions.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn delete_all_user_sessions(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "DELETE FROM lntl.sessions
WHERE user_id = $1
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_user_session` query
/// defined in `./src/lntl_server/sql/create_user_session.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateUserSessionRow {
  CreateUserSessionRow(session_id: String)
}

/// Runs the `create_user_session` query
/// defined in `./src/lntl_server/sql/create_user_session.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_user_session(db, arg_1, arg_2) {
  let decoder = {
    use session_id <- decode.field(0, decode.string)
    decode.success(CreateUserSessionRow(session_id:))
  }

  "INSERT INTO lntl.sessions (
  session_id,
  user_id
) VALUES (
  $1,
  $2
)
RETURNING session_id
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_user` query
/// defined in `./src/lntl_server/sql/create_user.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateUserRow {
  CreateUserRow(id: String)
}

/// Runs the `create_user` query
/// defined in `./src/lntl_server/sql/create_user.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_user(
  db,
  arg_1,
  arg_2,
  arg_3,
  arg_4,
  arg_5,
  arg_6,
  arg_7,
  arg_8,
  arg_9,
  arg_10,
) {
  let decoder =
  {
    use id <- decode.field(0, decode.string)
    decode.success(CreateUserRow(id:))
  }

  "INSERT INTO lntl.users (
  id, first_name, last_name, username,
  password_hash, dob_day, dob_month, dob_year,
  gender, pronouns, is_authenticated, created_at
) VALUES (
  $1, $2, $3, $4,
  $5, $6, $7, $8,
  $9, $10, TRUE, now()
)
RETURNING id
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.text(arg_5))
  |> pog.parameter(pog.int(arg_6))
  |> pog.parameter(pog.int(arg_7))
  |> pog.parameter(pog.int(arg_8))
  |> pog.parameter(pog.text(arg_9))
  |> pog.parameter(pog.text(arg_10))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_new_room` query
/// defined in `./src/lntl_server/sql/create_new_room.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateNewRoomRow {
  CreateNewRoomRow(id: String)
}

/// Runs the `create_new_room` query
/// defined in `./src/lntl_server/sql/create_new_room.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_new_room(db, arg_1, arg_2, arg_3, arg_4, arg_5) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(CreateNewRoomRow(id:))
  }

  "INSERT INTO lntl.rooms (
  id, owner_id, name, capacity, status, created_at, updated_at
) VALUES (
  $1, $2, $3, $4, $5, now(), now()
)
RETURNING id
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.int(arg_4))
  |> pog.parameter(pog.text(arg_5))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_new_message` query
/// defined in `./src/lntl_server/sql/create_new_message.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.4 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateNewMessageRow {
  CreateNewMessageRow(id: String)
}

/// Runs the `create_new_message` query
/// defined in `./src/lntl_server/sql/create_new_message.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_new_message(db, arg_1, arg_2, arg_3, arg_4, arg_5) {
  let decoder = {
    use id <- decode.field(0, decode.string)
    decode.success(CreateNewMessageRow(id:))
  }

  "INSERT INTO lntl.messages (
  id,
  room_id,
  user_id,
  content,
  status
) VALUES (
  $1,
  $2,
  $3,
  $4,
  $5 
)
RETURNING id
;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.text(arg_5))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `add_user_to_room` query
/// defined in `./src/lntl_server/sql/add_user_to_room.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.4 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_user_to_room(db, arg_1, arg_2) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "INSERT INTO lntl.room_members (
  room_id, user_id, joined_at
) VALUES (
  $1, $2, now()
)
;"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}
