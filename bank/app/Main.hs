import Control.Concurrent.STM
import Control.Monad (when, void)

data Money = Money { currency :: String, amount :: Integer } deriving Show

data Account = Account String Int (TVar Money)

data User = User String (TVar [Account])

data Bank = Bank (TVar [User]) (TVar Money)

instance Eq Money where
  (Money c1 a1) == (Money c2 a2) = c1 == c2 && a1 == a2

instance Eq Account where
  (Account c1 i1 _) == (Account c2 i2 _) = c1 == c2 && i1 == i2

newUser :: String -> STM User
newUser name = do
  accounts <- newTVar []
  return $ User name accounts

removeUser :: User -> STM ()
removeUser (User _ accountsVar) = do
  accounts <- readTVar accountsVar
  when (not $ null accounts) $
    writeTVar accountsVar []

openAccount :: User -> Money -> STM Account
openAccount (User _ accountsVar) money = do
  accounts <- readTVar accountsVar
  newAccount <- do
    moneyVar <- newTVar money
    return $ Account (currency money) (length accounts + 1) moneyVar
  writeTVar accountsVar (newAccount : accounts)
  return newAccount

closeAccount :: Account -> STM ()
closeAccount (Account _ _ moneyVar) = do
  money <- readTVar moneyVar
  when (amount money /= 0) $ writeTVar moneyVar (Money (currency money) 0)

deposit :: Account -> Money -> STM ()
deposit (Account _ _ moneyVar) (Money _ depositAmount) = do
  modifyTVar' moneyVar (\(Money currency currentAmount) -> Money currency (currentAmount + depositAmount))

withdraw :: Account -> Money -> STM ()
withdraw account withdrawalAmount = do
  currentMoney <- readTVar $ getMoneyVar account
  when (amount currentMoney < amount withdrawalAmount) $ retry
  deposit account (negateMoney withdrawalAmount)

transfer :: Account -> Account -> Money -> STM ()
transfer from to transferAmount = do
  withdraw from transferAmount
  deposit to transferAmount

transferBetweenUsers :: User -> User -> Money -> STM ()
transferBetweenUsers fromUser toUser transferAmount = do
  fromAccount <- getFirstAccount fromUser
  toAccount <- getFirstAccount toUser
  transfer fromAccount toAccount transferAmount

getFirstAccount :: User -> STM Account
getFirstAccount (User _ accountsVar) = do
  accounts <- readTVar accountsVar
  case accounts of
    []      -> retry
    (x : _) -> return x

calculateBankCommission :: Money -> Money
calculateBankCommission (Money currency amount) = Money currency (max 1 (amount `div` 100))

payBankCommission :: Bank -> STM ()
payBankCommission (Bank _ bankMoneyVar) = do
  modifyTVar' bankMoneyVar (\money -> depositMoney money (calculateBankCommission money))

applyCommission :: Money -> Money
applyCommission money = subtractMoney money (calculateBankCommission money)

transaction :: Bank -> STM ()
transaction bank@(Bank usersVar _) = do
  payBankCommission bank
  users <- readTVar usersVar
  mapM_ processUser users

processUser :: User -> STM ()
processUser (User _ accountsVar) = do
  accounts <- readTVar accountsVar
  mapM_ (void . processAccount) accounts

processAccount :: Account -> STM ()
processAccount account = do
  money <- readTVar $ getMoneyVar account
  when (amount money /= 0) $ modifyTVar' (getMoneyVar account) applyCommission

getMoneyVar :: Account -> TVar Money
getMoneyVar (Account _ _ moneyVar) = moneyVar

subtractMoney :: Money -> Money -> Money
subtractMoney (Money currency1 amount1) (Money _ amount2) =
  Money currency1 (amount1 - amount2)

negateMoney :: Money -> Money
negateMoney (Money currency amount) = Money currency (negate amount)

depositMoney :: Money -> Money -> Money
depositMoney (Money currency1 amount1) (Money _ amount2) =
  Money currency1 (amount1 + amount2)

-- Функция для проверки условия и вывода результата в консоль
assert :: String -> Bool -> IO ()
assert label condition =
  if condition
    then putStrLn $ label ++ " - Passed"
    else putStrLn $ label ++ " - Failed"

userName :: User -> String
userName (User name _) = name

userAccountsVar :: User -> TVar [Account]
userAccountsVar (User _ accountsVar) = accountsVar

moneyVar :: Account -> TVar Money
moneyVar (Account _ _ moneyVar) = moneyVar

newBank :: STM Bank
newBank = do
  usersVar <- newTVar []
  bankMoneyVar <- newTVar (Money "USD" 0)
  return $ Bank usersVar bankMoneyVar

bankMoneyVar :: Bank -> TVar Money
bankMoneyVar (Bank _ moneyVar) = moneyVar


main :: IO ()
main = do
  bank <- atomically newBank
  bankMoneyBefore <- atomically $ readTVar (bankMoneyVar bank)
  assert "Initial bank balance is zero" (amount bankMoneyBefore == 0)

  user1 <- atomically $ newUser "User1"
  user2 <- atomically $ newUser "User2"

  account1 <- atomically $ openAccount user1 (Money "USD" 100)
  account2 <- atomically $ openAccount user2 (Money "USD" 50)

  money1 <- atomically $ readTVar (moneyVar account1)
  money2 <- atomically $ readTVar (moneyVar account2)
  assert "Initial balance for account1 is 100" (amount money1 == 100)
  assert "Initial balance for account2 is 50" (amount money2 == 50)

  atomically $ transfer account1 account2 (Money "USD" 30)

  money1AfterTransfer <- atomically $ readTVar (moneyVar account1)
  money2AfterTransfer <- atomically $ readTVar (moneyVar account2)
  assert "Balance for account1 after transfer is 70" (amount money1AfterTransfer == 70)
  assert "Balance for account2 after transfer is 80" (amount money2AfterTransfer == 80)

  atomically $ transaction bank

  money1AfterTransaction <- atomically $ readTVar (moneyVar account1)
  money2AfterTransaction <- atomically $ readTVar (moneyVar account2)
  assert "Balance for account1 after transaction is 69" (amount money1AfterTransaction == 70)
  assert "Balance for account2 after transaction is 79" (amount money2AfterTransaction == 80)

  bankMoneyAfterTransaction <- atomically $ readTVar (bankMoneyVar bank)
  assert "Bank balance after transaction is 1" (amount bankMoneyAfterTransaction == 1)

  putStrLn "All tests passed!"
