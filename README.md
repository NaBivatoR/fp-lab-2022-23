## Участие в курса


TODO: teams???

Курсът има чат канали във ФМИ Teams-а. Там ще се
* пускат обявления свързани с курса
* обсъждат неофициално неща по курса публично
* обсъждат неофициално неща по курса на лични съобщения с мен

Ще пусна линк покана за Discord-а в Moodle.

Ако някой няма достъп до Moodle курса или иска да участва без да е записал курса, моля да ми пише мейл.

За да участвате в курса, трябва да се свържете с мен за да предприема действия:
* включване в "група" в Discord с която да мога да ви сръчквам за обявления
* взимане на данни нужни за подготовка за домашни и сформиране на таблица с оценки

Това се случва като ми пишете на лично съобщение в Discord, в следния формат:
```
<фн>,<две имена на кирилица>,<github потребителско име>
```

## Контакти

* Mail - godzbanebane@gmail.com
* TODO: teams
* Twitter - @googleson78

## Взимане на курса

Курсът се взима с домашни и проект.

Домашните не са задължителни, проектът е.

Имането на проект **не гарантира** взимането на предмета.
Пример за кога би се случило това е ако не напишете нито едно домашно и изберете да правите проект, който дава минималния брой точки.

Домашните ще са между 3 и 7 на брой.

Домашните ще се предават в лично github хранилище за всеки човек чрез pull request.

(TODO) споделяне на проекта с главната дисциплина.

Проектът се предава по същия начин както домашните - pull request във вашето хранилище в github, по който ви пиша обратна връзка.

## Технически детайли

### Инсталиране на инструменти за работа с Haskell

Имате [няколко варианта](https://www.haskell.org/downloads/).

Препоръчваният начин е да използвате [`ghcup`][ghcup] - инструмент за менежиране на инструменти свързани с `Haskell`.

На [началната страница на `ghcup`][ghcup] има едноредови инструкции за инсталирането на `ghcup`, заедно с gif, демонстриращ как да го използвате, за да инсталирате инструменти.

Алтернативно можете да разгледате [по-подробните инструкции](https://www.haskell.org/ghcup/install/) за инсталиране на `ghcup`.

След като го инсталирате, може да изпълните
```
> ghcup install ghc
```
или алтернативно, `ghcup tui`, след което можете с текстови интърфейс да си изберете какво искате да инсталирате.

След това, моля проверете дали работи всичко като изпълните командата `ghci` в терминал.

[Кратко разглеждане на основните `Haskell` инструменти, инсталирани чрез `ghcup`](https://www.haskell.org/ghcup/steps/)


[ghcup]: https://www.haskell.org/ghcup/

### Редактор и интеграция с Haskell

Препоръчаният метод за работа с Haskell е [VSCode](https://code.visualstudio.com/) заедно с [HLS](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) разширението.

То включва много полезни функционалности, така че е хубаво ако ви е интересно да се запознаете повече с документацията му/питате мен въпроси за него.

Важно е да се отбележи, че докато работим с файл извън "проект" (което ще правим повечето време), е **нужно** да имаме инсталирано `ghc` и да е в `PATH` за да работи HLS

Нямам против да ползвате <моя-любим-редактор> - аз ще ползвам `vim` докато ви показвам неща, като най-вероятно можете да си нагласите и HLS да работи с <моя-любим-редактор>. В такъв случай, можете лесно да се сдобиетe със HLS чрез `ghcup`.

Ако решите да не ползвате VSCode, трябва да измислим начин да си споделяте сесията в редактора си с мен, в случай че минем на дистанционно обучение.

## Haskell ресурси

* Донякъде плагиатствам от [този курс](https://github.com/bobatkey/CS316-2022)
* [Книгата](http://www.cs.nott.ac.uk/~pszgmh/pih.html) на която е базиран горният курс (и също я смятам за добър ресурс)
* [Силно препоръчително четиво][parse-dont-validate] свързано с
  * как да ни се налага да мислим по-малко докато програмираме
  * как да избягваме големи класове грешки с помощта на компилатора
  * ключова начин на мислене в Haskell
  * защо да програмираме на Haskell
* Хубав talk/demo за неща които често ти се налагат често и как се правят те в Haskell - [цък](https://www.youtube.com/watch?v=idU7GdlfP9Q)
* [Hoogle](https://hoogle.haskell.org/) - търсене за хаскел функции (идентификатори) (и по типове!)
* [Hackage](http://hackage.haskell.org/) - търсене за хаскел пакети
* [Real World Haskell](http://book.realworldhaskell.org/) - практично насочена

  Малко остаряла.

* Специализирани ресурси:

  * защо да правим `a -> Maybe b`, вместо `a -> Bool`:
    * [Parse, don't validate][parse-dont-validate]
    * https://runtimeverification.com/blog/code-smell-boolean-blindness/
    * https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
  * Мазохизъм/Програмиране с типове - [Thinking with Types](https://thinkingwithtypes.com/)
  * Паралелно и конкуретно програмиране - [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html)
    * една от въведителните части е доста добра за добиване на по-добро разбиране над оценителния модел на Haskell
  * Разглеждане на фундаментални типови класове - [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
  * Разглеждане на различни интересни библиотеки/разшиерния на езика - [24 days of *](https://ocharles.org.uk/)
  * Има **много** научни статии, които са доста лесно четими дори за начинаещи.

Съветвам ви директно да ме питате за повече ресурси ако ви интересува конкретна тема.


[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

---

Курсът се подкрепя от [Tweag](https://www.tweag.io/)


