/- # Functional Programming Exercise: Employee Data and Bonus Calculations -/

import Testing

-- ==========================
-- PART 1: DATA MODELING
-- ==========================

/-
  TASK 1.1: Define Engineering Units
  Create an inductive type `EngineeringUnit` with three constructors:
  Backend, Frontend, DevOps
  Don't forget to derive Repr and BEq
-/
inductive EngineeringUnit where
  | Backend
  | Frontend
  | Devops
deriving Repr, BEq

/-
  TASK 1.2: Define Sales Units
  Create an inductive type `SalesUnit` with two constructors:
  Internal, External
  Don't forget to derive Repr and BEq
-/
inductive SalesUnit where
  | Internal
  | External
  deriving Repr, BEq

/-
  TASK 1.3: Define Department
  Create an inductive type `Department` with three constructors:
  - Engineering (takes an EngineeringUnit parameter)
  - Sales (takes a SalesUnit parameter)
  - Marketing (no parameters)
  Don't forget to derive Repr and BEq
-/
inductive Department where
  | Engineering (eu: EngineeringUnit)
  | Sales (su: SalesUnit)
  | Marketing
  deriving Repr, BEq

/-
  TASK 1.4: Define Seniority Levels
  Create an inductive type `SeniorityLevel` with six constructors:
  Junior, Senior, Lead, Manager, Director, Board
  Don't forget to derive Repr and BEq
-/
inductive SeniorityLevel
  | Junior
  | Senior
  | Lead
  | Manager
  | Director
  | Board
  deriving Repr, BEq


/-
  TASK 1.5: Define Employee Structure
  Create a structure `Employee` with the following fields:
  - id: Nat
  - name: String
  - salary: Float (annual salary)
  - dept: Department (primary department)
  - seniority: SeniorityLevel
  - tenure: Nat (years with company)
  Don't forget to derive Repr and BEq
-/
structure Employee where
  id: Nat
  name: String
  salary: Float
  dept: Department
  seniority: SeniorityLevel
  tenure: Nat -- years with company
  deriving Repr, BEq

/-
  TASK 1.6: Define Company Structure
  Create a structure `Company` with fields:
  - employees: List Employee
  - departments: List Department
  Don't forget to derive Repr and BEq
-/
structure Company where
  employees: List Employee
  departments: List Department
  deriving Repr, BEq

/-
  TASK 1.7: Define Sale Structure
  Create a structure `Sale` with fields:
  - id: Nat
  - employeeId: Nat
  - amount: Float
  - date: String
  Don't forget to derive Repr and BEq
-/
structure Sale where
  id: Nat
  employeeId: Nat
  amount: Float
  date: String
  deriving Repr, BEq

/-
  TASK 1.8: Define DepartmentPerformance Structure
  Create a structure `DepartmentPerformance` with fields:
  - dept: Department
  - profitable: Bool
  Don't forget to derive Repr and BEq
-/
structure DepartmentPerformance where
  dept: Department
  profitable: Bool
  deriving Repr, BEq


/-
  TASK 1.9: Define YearlyReport Structure
  Create a structure `YearlyReport` with fields:
  - year: Nat
  - company: Company
  - totalProfit: Float
  - deptPerformance: List DepartmentPerformance
  - sales: List Sale
  Don't forget to derive Repr and BEq
-/
structure YearlyReport where
  year: Nat
  company: Company
  totalProfit: Float
  deptPerformance: List DepartmentPerformance
  sales: List Sale
  deriving Repr, BEq


-- ==========================
-- PART 2: EXAMPLE DATA
-- ==========================

/-
  TASK 2.1: Create Example Data
  Define a namespace `Examples` and create example data:

  Departments list should contain:
  - Engineering Backend
  - Marketing
  - Engineering Frontend
  - Sales Internal
  - Sales External

  Employees (use whichever syntax you prefer: mk, ⟨⟩, or {}):
  - alice: id=1, name="Alice", salary=80000, dept=Engineering Backend, seniority=Senior, tenure=5
  - bob: id=2, name="Bob", salary=60000, dept=Sales Internal, seniority=Junior, tenure=2
  - carol: id=3, name="Carol", salary=90000, dept=Engineering Frontend, seniority=Lead, tenure=7
  - david: id=4, name="David", salary=130000, dept=Marketing, seniority=Board, tenure=3
  - eve: id=5, name="Eve", salary=85000, dept=Sales Internal, seniority=Manager, tenure=6

  Company: combine employees list and departments list

  Department performances (in order of departments list):
  - Engineering Backend: profitable=true
  - Marketing: profitable=true
  - Engineering Frontend: profitable=false
  - Sales Internal: profitable=true
  - Sales External: profitable=false

  Sales data:
  - Sale 1: Bob, $50,000, "2024-01-15"
  - Sale 2: Eve, $75,000, "2024-02-20"
  - Sale 3: Bob, $30,000, "2024-03-10"
  - Sale 4: Eve, $45,000, "2024-04-05"
  - Sale 5: Carol, $20,000, "2024-05-12"
  - Sale 6: Alice, $15,000, "2024-06-18"
  - Sale 7: Eve, $60,000, "2024-07-22"

  Yearly report for 2024 with totalProfit = 1000000
-/

namespace Examples

-- if we uncomment the next line we can use Backend instead of EngineeringUnit.Backend
-- open Department EngineeringUnit SalesUnit SeniorityLevel

def departments : List Department := [
  Department.Engineering (EngineeringUnit.Backend),
  Department.Marketing,
  Department.Engineering (EngineeringUnit.Frontend),
  Department.Sales (SalesUnit.Internal),
  Department.Sales (SalesUnit.External)
]

-- like here
open Department EngineeringUnit SalesUnit SeniorityLevel

def alice : Employee := {
  id := 1
  name := "Alice"
  salary := 80000
  dept := Engineering Backend
  seniority := Senior
  tenure := 5
}

def bob : Employee := {
  id := 2
  name := "Bob"
  salary := 60000
  dept := Sales Internal
  seniority := Junior
  tenure := 2
}

def carol : Employee := {
  id := 3
  name := "Carol"
  salary := 90000
  dept := Engineering Frontend
  seniority := Lead
  tenure := 7
}

def david : Employee := {
  id := 4
  name := "David"
  salary := 130000
  dept := Marketing
  seniority := Board
  tenure := 3
}

def eve : Employee := {
  id := 5
  name := "Eve"
  salary := 85000
  dept := Sales Internal
  seniority := Manager
  tenure := 6
}

def employees: List Employee := [
  alice, bob, carol, david, eve
]

def company: Company := {
  departments := departments
  employees := employees
}

def performances : List DepartmentPerformance := [
    ⟨departments[0], true⟩,
    ⟨departments[1], true⟩,
    ⟨departments[2], false⟩,
    ⟨departments[3], true⟩,
    ⟨departments[4], false⟩
]

def sales : List Sale := [
  ⟨1, 2, 50000, "2024-01-15"⟩,
  Sale.mk 2 5 75000 "2024-02-20",
  { id := 3, employeeId := 1, amount := 30000, date := "2024-03-10" },
  ⟨4, 5, 45000, "2024-04-15"⟩,
  ⟨5, 3, 20000, "2024-05-12"⟩,
  ⟨6, 1, 15000, "2024-06-18"⟩,
  ⟨7, 5, 60000, "2024-07-20"⟩,
]

def yearlyreport : YearlyReport := {
  year := 2024,
  company := company,
  totalProfit := 1000000,
  deptPerformance := performances,
  sales := sales
}


end Examples


-- ==========================
-- PART 3: BONUS CALCULATIONS
-- ==========================

/-
  TASK 3.1: Seniority Bonus (Version 1)
  Implement `seniorityBonus1` using nested if-then-else statements.
  Rules:
  - Managers: 5% of salary
  - Directors: 15% of salary
  - Board members: 40% of salary
  - All others: 0%
-/
open SeniorityLevel
def seniorityBonus (seniority: SeniorityLevel) (salary: Float): Float :=
  if seniority == Manager then 0.05 * salary
  else if seniority == Director then 0.15 * salary
  else if seniority == Board then 0.4 * salary
  else 0

/-
  TASK 3.2: Seniority Bonus (Version 2)
  Implement `seniorityBonus2` by first computing the rate, then multiplying by salary.
  This reduces code duplication. Use a let binding for the rate.
-/
def seniorityBonus2 (e : Employee) : Float :=
  let rate :=
    if e.seniority == Manager then 0.05
    else if e.seniority == Director then 0.15
    else if e.seniority == Board then 0.4
    else 0.0
  rate * e.salary


/-
  TASK 3.3: Seniority Bonus (Version 3)
  Implement `seniorityBonus3` using pattern matching on e.seniority.
  Use a wildcard pattern _ for the default case.
  HINT: Open SeniorityLevel to avoid qualified names
-/
def seniorityBonus3 (e : Employee) : Float :=
  match e.seniority with
    | Manager => 0.05 * e.salary
    | Director => 0.15 * e.salary
    | Board => 0.4 * e.salary
    | _ => 0


/-
  TASK 3.4: Bonus Distribution Rate
  Implement `bonusDistribution` that returns the percentage each department
  redistributes to employees:
  - Engineering: 10%
  - Sales: 8%
  - Marketing: 3%
  Use pattern matching on the department.
-/
open Department
def bonusDistribution (d: Department) :=
  match d with
    | Engineering _ => 0.1
    | Sales _ => 0.08
    | Marketing => 0.3


/-
  TASK 3.5: Department Bonus Share
  Implement `departmentBonusShare` that calculates how much bonus each department gets.
  Formula: (totalProfit * 0.1) / number of departments
  HINT: Use .length.toFloat to convert list length to Float
-/
open YearlyReport
def departmentBonusShare (yearly: YearlyReport): Float :=
  (yearly.totalProfit * 0.1) / (yearly.company.departments.length.toFloat)


/-
  TASK 3.6: Department Employees
  Implement `departmentEmployees` that finds all employees in a given department.
  Use the filter function on the company's employee list.
-/
open Company
def departmentEmployees (company: Company) (d: Department): List Employee :=
  company.employees |> List.filter (fun emp => emp.dept == d)


/-
  TASK 3.7: Is Department Profitable
  Implement `isDepartmentProfitable` that checks if a department was profitable.
  Use find? on report.deptPerformance and check if result is Some.
  HINT: Use Option.isSome and the pipeline operator |>
-/
def isDepartmentProfitable (d: Department) (yearly: YearlyReport): Bool :=
  yearly.deptPerformance.find? (fun p => p.profitable && p.dept == d) |> Option.isSome

/-
  TASK 3.8: Department Bonus
  Implement `departmentBonus` that calculates an employee's department bonus.
  Steps:
  1. Get the department's share using departmentBonusShare
  2. Calculate bonus potential: share * bonusDistribution for the employee's dept
  3. Set bonus to bonusPotential if department is profitable, otherwise 0
  4. Find all employees in the department
  5. Divide bonus equally among department employees
-/
def departmentBonus (e: Employee) (yearly: YearlyReport): Float :=
  let share := departmentBonusShare yearly
  let potential := share * bonusDistribution e.dept
  let bonus := if isDepartmentProfitable e.dept yearly then potential else 0
  let empsInDept := departmentEmployees yearly.company e.dept
  if bonus > 0 then bonus / empsInDept.length.toFloat else 0

/-
  TASK 3.9: Department Assignment Structure
  Define a structure `DepartmentAssignment` with one field:
  - of: Employee → List Department
  This maps each employee to their list of departments.
-/
structure DepartmentAssignment where
  of: Employee -> List Department

/-
  TASK 3.10: Department Bonus (Multiple Departments)
  Implement `departmentBonus2` for employees who belong to multiple departments.
  Steps:
  1. Get the department share
  2. Define a local function `bonus` that calculates bonus for one department
  3. Map this function over all departments the employee belongs to
  4. Sum all the bonuses
  HINT: Use assignment.of e to get employee's departments, then map and sum
-/
def departmentBonus2 (e : Employee) (report : YearlyReport) (assignment : DepartmentAssignment) : Float :=
  let share := departmentBonusShare report
  let bonus (d: Department): Float :=
    let potential := share * bonusDistribution d
    let bonus := if isDepartmentProfitable d report then potential else 0
    let empsInDept := departmentEmployees report.company d
    if bonus > 0 then bonus / empsInDept.length.toFloat else 0
  let bonuses := assignment.of e |>.map bonus
  bonuses.sum


-- ==========================
-- TESTS (Uncomment after implementing)
-- ==========================

#assert (seniorityBonus3 Examples.alice) == 0.0
#assert (seniorityBonus3 Examples.bob) == 0.0
#assert (seniorityBonus3 Examples.carol) == 0.0
#assert (seniorityBonus3 Examples.david) == 52000.0
#assert (seniorityBonus3 Examples.eve) == 4250.0

#assert (departmentBonus Examples.bob Examples.yearlyreport) == (departmentBonus Examples.eve Examples.yearlyreport)
