create table users
  ( id bigserial primary key

  , username varchar(255) not null
  , passwd char(209) not null
  -- argon2 with saltlen 64 byte

  , email varchar(255) not null
  , verified boolean not null

  , last_login timestamp without time zone not null check (last_login < now())

  , failed_login_attempts smallint not null check (0 <= failed_login_attempts)
  , locked_until timestamp without time zone not null check (now() < locked_until)
  , unique (email)
  );
create index username on users(username);

-- create table session_data
--   ( id bigserial primary key
--   , user_ bigint references users(id) not null
--   , xsrf_token varchar()
--   );

create table user_data
  ( id bigserial primary key
  , user_ bigint references users(id) not null
  , height_cm smallint check(0 < height_cm)
  , exercise_level double precision
                   check ( 1.2 <= exercise_level and
                                  exercise_level <= 1.9)
    -- https://en.wikipedia.org/wiki/Harris%E2%80%93Benedict_equation
  , day_of_birth date check (day_of_birth < now())
  , unique (user_)
  );

create table sleep
  ( id bigserial primary key
  , user_ bigint references users(id) not null
  , time_ timestamp without time zone not null
  , amount interval check ('0' <= amount)
  , unique (user_, time_)
  );

create table exercise
  ( id bigserial primary key
  , user_ bigint references users(id) not null
  , time_  timestamp without time zone not null
  , amount interval check ('0' <= amount)
  , unique (user_, time_)
  );

create table weight
  ( id bigserial primary key
  , user_ bigint references users(id) not null
  , time_  timestamp without time zone not null
  , amount double precision check (0 <= amount)
  , unique (user_, time_)
  );

create table user_goals
  ( id bigserial primary key
  , user_ bigint references users(id) not null
  , set_at         timestamp without time zone not null
  , weight         double precision   check ( 0  <= weight)
  , kcal           double precision   check ( 0  <= kcal  )
  , liquid_per_day smallint default 6 check ( 0  <= liquid_per_day)
  , fruit_per_day  smallint default 2 check ( 0  <= fruit_per_day )
  , veg_per_day    smallint default 3 check ( 0  <= veg_per_day   )
  , carbs_per_day  smallint default 4 check ( 0  <= carbs_per_day )
  , milk_per_day   smallint default 3 check ( 0  <= milk_per_day  )
  , nuts_per_day   smallint default 2 check ( 0  <= nuts_per_day  )
  , sweets_per_day smallint default 1 check ( 0  <= sweets_per_day)
  , tofu_per_week  smallint default 4 check ( 0  <= tofu_per_week )
  , meat_per_week  smallint default 3 check ( 0  <= meat_per_week )
  , fish_per_week  smallint default 2 check ( 0  <= fish_per_week )
  , eggs_per_week  smallint default 4 check ( 0  <= eggs_per_week )
  , sleep          interval           check ('0' <= sleep)
  , exercise       interval           check ('0' <= exercise)
  , check ( (weight         is not null) or (kcal           is not null)
         or (liquid_per_day is not null) or (fruit_per_day  is not null)
         or (veg_per_day    is not null) or (carbs_per_day  is not null)
         or (milk_per_day   is not null) or (nuts_per_day   is not null)
         or (sweets_per_day is not null) or (tofu_per_week  is not null)
         or (meat_per_week  is not null) or (fish_per_week  is not null)
         or (eggs_per_week  is not null) or (sleep          is not null)
         or (exercise       is not null))
  , unique (user_, set_at)
  );

create table meal
  ( id bigserial primary key
  , user_ bigint references users(id) not null
  , time_ timestamp without time zone not null
  , name_ varchar(255)
  , note  varchar
  , kcal   real     check (0 <= kcal  )
  , liquid smallint check (0 <= liquid)
  , fruit  smallint check (0 <= fruit )
  , veg    smallint check (0 <= veg   )
  , carbs  smallint check (0 <= carbs )
  , milk   smallint check (0 <= milk  )
  , nuts   smallint check (0 <= nuts  )
  , sweets smallint check (0 <= sweets)
  , tofu   smallint check (0 <= tofu  )
  , meat   smallint check (0 <= meat  )
  , fish   smallint check (0 <= fish  )
  , eggs   smallint check (0 <= eggs  )
  , unique (user_, time_)
  );
