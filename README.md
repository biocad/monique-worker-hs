# monique-worker

## Реализация

Для того, чтобы делать какую-либо полезную работу, воркер должен проеобразовывать конфиг в некоторый результат.
То есть, воркер должен реализовывать функцию со следующим типом (`src/Network/Monique/Worker/Internal/Types.hs`):

``` haskell
type Processing a = UserId -> a -> ExceptT MoniqueError IO WorkerResult
```
где:
  * `a` - конфиг, который приходит от внешнего мира. Должен быть преставителем класса `FromJSON (Data.Aeson)`;
  * `UserId` - id пользователя (имеет тип `Text`);
  * `MoniqueError` - тип ошибки, если работа воркера закончилась неудачей;
  * `WorkerResult` - результат, если воркер отработал в штатном режиме.
  
### Генерация ошибки
Если воркер закончился неудачей, ошибку типа `MoniqueError` можно кинуть следующей функцией:
``` haskell
throwWorkerError :: WorkerName -> String -> ExceptT MoniqueError IO WorkerResult
```
где `type WorkerName = String`.

### Генерация результата
Если воркер завершился в штатном режиме, то результатом должен быть `WorkerResult`:
``` haskell
data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)
```
где 

``` haskell
data TaskResult  = TaskResult { version :: Int
                              , content :: Value
                              }

type UType = Text
type UData = Value                         
```
a `Value` - объект JSON (из библиотеки `Data.Aeson`). 


## Запуск
Для того, чтобы сделать воркера исполняемым, нужно воспользоваться функцией
```haskell
runApp :: FromJSON a => Processing a -> IO ()
```
где в качестве аргумента передать реализованную вами функцию с типом `Processing a`.

После того, как будет сгенерирован исполняемый файл воркера, можно будет его запустить с ключом `--help`.
Будет напечатана информация с параметрами, которые нужно передать для запуска воркера.



