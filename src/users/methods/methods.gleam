import users/types/users

pub fn create_user(
  users_id id: String,
  first_name name_f: String,
  last_name name_l: String,
  username uname: String,
  is_user_auth auth: Bool,
  pronouns_singular sl: String,
  pronouns_plural pl: String,
  dob_day day: Int,
  dob_week month: Int,
  dob_month year: Int,
  user_gender gender: users.Gender,
) -> users.User {
  users.User(
    name: users.Name(name_f, name_l),
    username: users.UserName(uname),
    user_id: users.UserId(id),
    user_auth: auth,
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
