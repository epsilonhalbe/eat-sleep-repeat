{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module ESR.Database where

import           Data.Int            (Int16)
import           Data.Text           (Text)
import           Data.Time           (Day, NominalDiffTime, UTCTime)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Database.Persist.Postgresql.Orphans ()


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  username     Text    sql=username      sqltype=varchar(255)
  password     Text    sql=passwd        sqltype=char(209)     -- argon2 with saltlen 64 byte

  email        Text    sql=email         sqltype=varchar(255)
  verified     Bool    sql=verified      sqltype=boolean      default=false

  lastLogin    UTCTime sql=last_login    sqltype="timestamp without time zone"

  failedLogins Int16   sql=failed_logins sqltype=smallint default=0
  lockedUntil  UTCTime sql=locked_until  sqltype="timestamp without time zone"

  UniqueEmail email

UserData sql=user_data
  user          UserId       sql=user_          sqltype=bigint
  height        Int16  Maybe sql=height_cm      sqltype=smallint
  exerciseLevel Double Maybe sql=exercise_level sqltype="double precision"
  dayOfBirth    Day    Maybe sql=day_of_birth   sqltype=date
  UniqueUser user

Sleep sql=sleep
  user   UserId                sql=user_ sqltype=bigint
  time   UTCTime               sql=time_ sqltype="timestamp without time zone"
  amount NominalDiffTime Maybe sql=amount sqltype=interval
  UniqueSleepUserTime user time

Exercise sql=exercise
  user   UserId                sql=user_  sqltype=bigint
  time   UTCTime               sql=time_  sqltype="timestamp without time zone"
  amount NominalDiffTime Maybe sql=amount sqltype=interval
  UniqueExerciseUserTime user time

Weight sql=weight
  user   UserId        sql=user_  sqltype=bigint
  time   UTCTime       sql=time_  sqltype="timestamp without time zone"
  amount Double  Maybe sql=amount sqltype="double precision"
  UniqueWeightUserTime user time

UserGoals sql=user_goals
  user           UserId                sql=user_ sqltype=bigint references users(id)
  setAt          UTCTime               sql=set_at sqltype="timestamp without time zone"
  weight         Double          Maybe sql=weight sqltype="double precision"
  kcal           Double          Maybe sql=kcal sqltype="double precision"
  liquidPerDay   Int16           Maybe sql=liquid_per_day sqltype=smallint default=6
  fruitPerDay    Int16           Maybe sql=fruit_per_day  sqltype=smallint default=2
  vegPerDay      Int16           Maybe sql=veg_per_day    sqltype=smallint default=3
  carbsPerDay    Int16           Maybe sql=carbs_per_day  sqltype=smallint default=4
  milkPerDay     Int16           Maybe sql=milk_per_day   sqltype=smallint default=3
  nutsPerDay     Int16           Maybe sql=nuts_per_day   sqltype=smallint default=2
  sweetsPerDay   Int16           Maybe sql=sweets_per_day sqltype=smallint default=1
  tofuPerWeek    Int16           Maybe sql=tofu_per_week  sqltype=smallint default=4
  meatPerWeek    Int16           Maybe sql=meat_per_week  sqltype=smallint default=3
  fishPerWeek    Int16           Maybe sql=fish_per_week  sqltype=smallint default=2
  eggsPerWeek    Int16           Maybe sql=eggs_per_week  sqltype=smallint default=4
  sleep          NominalDiffTime Maybe sql=sleep          sqltype=interval
  exercise       NominalDiffTime Maybe sql=exercise       sqltype=interval
  UniqueUserSetAt user setAt

Meal sql=meal
  user   UserId       sql=user_  sqltype=bigint
  time   UTCTime      sql=time_  sqltype="timestamp without time zone"
  name   Text         sql=name_  sqltype=varchar(255)
  note   Text   Maybe sql=note   sqltype=text
  kcal   Double Maybe sql=kcal   sqltype="double precision"
  liquid Int16  Maybe sql=liquid sqltype=smallint
  fruit  Int16  Maybe sql=fruit  sqltype=smallint
  veg    Int16  Maybe sql=veg    sqltype=smallint
  carbs  Int16  Maybe sql=carbs  sqltype=smallint
  milk   Int16  Maybe sql=milk   sqltype=smallint
  nuts   Int16  Maybe sql=nuts   sqltype=smallint
  sweets Int16  Maybe sql=sweets sqltype=smallint
  tofu   Int16  Maybe sql=tofu   sqltype=smallint
  meat   Int16  Maybe sql=meat   sqltype=smallint
  fish   Int16  Maybe sql=fish   sqltype=smallint
  eggs   Int16  Maybe sql=eggs   sqltype=smallint
  UniqueMealUserTime user time
|]
