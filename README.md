# monique-worker

## Реализация

Для того, чтобы делать какую-либо полезную работу, воркер должен проеобразовывать конфиг в некоторый результат.
То есть, воркер должен реализовывать функцию со следующим типом:

``` haskell
type Processing a = UserId -> a -> ExceptT MoniqueError IO WorkerResult
```

Разберём:
  * UserId -  Еуче 
  * a - конфиг, который приходит от пользователя. Должен быть преставителем класса `FromJSON (Data.Aeson)`.

``` haskell
data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

type Processing a = UserId -> a -> ExceptT MoniqueError IO WorkerResult
```


