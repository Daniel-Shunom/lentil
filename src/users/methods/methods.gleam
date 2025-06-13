import gleam/bit_array
import gleam/crypto.{Md5}
import gleam/int
import prng/random
import prng/seed
import users/types/users

pub fn create_user(
  first_name name_f: String,
  last_name name_l: String,
  username uname: String,
  pronouns_singular sl: String,
  pronouns_plural pl: String,
  dob_day day: Int,
  dob_month month: Int,
  dob_year year: Int,
  user_gender gender: users.Gender,
) -> users.User {
  let id = generate_user_id()
  users.User(
    name: users.Name(name_f, name_l),
    username: users.UserName(uname),
    user_id: users.UserId(id),
    user_auth: False,
    user_dob: users.DOB(day, month, year),
    user_gender: gender,
    user_pronouns: users.Pronouns(sl, pl),
  )
}

pub fn get_user_id(user: users.User) -> users.UserId {
  user.user_id
}

pub fn get_name(user: users.User) -> users.Name {
  user.name
}

pub fn get_username(user: users.User) -> users.UserName {
  user.username
}

pub fn get_dob(user: users.User) -> users.DOB {
  user.user_dob
}

pub fn get_user_gender(user: users.User) -> users.Gender {
  user.user_gender
}

pub fn get_pronouns(user: users.User) -> users.Pronouns {
  user.user_pronouns
}

pub fn update_name(
  new_fname fname: String,
  new_lname lname: String,
  user user: users.User,
) -> users.User {
  users.User(..user, name: users.Name(fname, lname))
}

pub fn update_username(
  new_username uname: String,
  user user: users.User,
) -> users.User {
  users.User(..user, username: users.UserName(uname))
}

pub fn update_user_gender(
  new_gender gender: users.Gender,
  user user: users.User,
) -> users.User {
  users.User(..user, user_gender: gender)
}

pub fn update_user_pronouns(
  new_singular_pronoun nsp: String,
  new_plural_pronoun npp: String,
  user user: users.User,
) -> users.User {
  users.User(..user, user_pronouns: users.Pronouns(nsp, npp))
}

fn generate_user_id() -> String {
  let thasher = fn(str: String) {
    bit_array.from_string(str)
    |> crypto.hash(Md5, _)
    |> bit_array.base16_encode()
  }
  let str = random.fixed_size_string(32)
  let num = random.int(0, 100_000)
  let secure_prefix =
    random.int(0, 13)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(num, _)
    |> int.to_string()

  let secure_id =
    random.int(0, 9)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(str, _)

  "lntl-user-" <> thasher(secure_prefix <> secure_id)
}
