pub type UserId {
  UserId(id: String)
}

pub type Name {
  Name(first: String, last: String)
}

pub type DOB {
  DOB(day: Int, month: Int, year: Int)
}

pub type Pronouns {
  Pronouns(singular: String, plural: String)
}

pub type UserName {
  UserName(username: String)
}

pub type Gender {
  CisGender
  TransGender
  NonBinary
  Other
}

pub type User {
  User(
    name: Name,
    username: UserName,
    user_id: UserId,
    user_auth: Bool,
    user_dob: DOB,
    user_gender: Gender,
    user_pronouns: Pronouns,
  )
}
