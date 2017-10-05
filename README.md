# monique-worker

## Реализация

Для того, чтобы делать какую-либо полезную работу, воркер должен проеобразовывать конфиг в некоторый результат.
То есть, воркер должен реализовывать функцию со следующим типом (`src/Network/Monique/Worker/Internal/Types.hs`):

``` haskell
type Processing a s = UserId -> WorkerName -> a -> StateT s IO (Except MoniqueError WorkerResult)
```
где:
  * `a` - конфиг, который приходит от внешнего мира. Должен быть преставителем класса `FromJSON & ToJSON (Data.Aeson)`;
  * `s` - хранимое состояние контроллера (используется в монаде `StateT`);
  * `UserId` - id пользователя (имеет тип `Text`);
  * `WorkerName` - обычная строка с именем воркера (вдруг вам захочется ошибку кинуть или дополнительный лог сделать?);
  * `MoniqueError` - тип ошибки, если работа воркера закончилась неудачей;
  * `WorkerResult` - результат, если воркер отработал в штатном режиме.
  
### Генерация ошибки
Если воркер закончился неудачей, ошибку типа `MoniqueError` можно кинуть следующей функцией:
``` haskell
throwWorkerError :: WorkerName -> String -> StateT s IO (Except MoniqueError WorkerResult)
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

Пример для обычного воркера (`ExampleA.hs`) и воркера с простейшим состоянием (`ExampleB.hs`) можно найти в папке `app`.


## Запуск
Для того, чтобы сделать воркера исполняемым, нужно воспользоваться функцией
```haskell
runApp :: FromJSON a => s -> Processing a s -> IO ()
```
где в качестве аргумента передать:
  * начальное состояние;
  * реализованную вами функцию с типом `Processing a s`.

После того, как будет сгенерирован исполняемый файл воркера, можно будет его запустить с ключом `--help`.
Будет напечатана информация с параметрами, которые нужно передать для запуска воркера.



