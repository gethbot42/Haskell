module DataTypes where

-- Types for Client data constructor.
type Name = String
type Position = String
type CompanyID = Int

-- Types for Person.
type FirstName = String
type LastName = String
type OnMailingList = Bool

-- Types for TimeMachine.
type Price = Double
type Make = String
type ModelNumber = Int

data Person = Person FirstName LastName Gender
  deriving Show

data Client = GovOrg Name
            | Company Name CompanyID Person Position
            | Individual Person OnMailingList
  deriving Show

data Gender = Male | Female | NonBinary | Unknown
  deriving Show

data TimeMachine = TimeMachine MachineType TravelType Price
  deriving Show

data MachineType = MachineType Make ModelNumber Name
  deriving Show

data TravelType = Past | Future | Both
  deriving Show

