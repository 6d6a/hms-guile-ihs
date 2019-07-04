Данный репозиторий предоставляет Guile IHS - интерфейс командной строки
к [API Majordomo](https://hms.majordomo.ru/api/).

Установка может быть выполнена добавлением
[Majordomo Guix channel](https://gitlab.wugi.info/guix/guix-majordomo)
после установки пакетного менеджера [Guix](https://guix.gnu.org/).

## Автодополнение

Все команды могут быть автодополнены с помощью <kbd>Tab</kbd>:

ihs <kbd>Tab</kbd>

ihs web <kbd>Tab</kbd>

ihs web unix ac_12345

## Фильтрация вывода

Вывод команд можно фильтровать с помощью `recsel`, например команда:
``` shell
ihs web website ac_12345 \
    | recsel -pname,document_root -e "ddos_protection='true'"
```
выведет имена и корневые директории сайтов аккаунта ac_12345, где включена
защита от DDoS атак.

recsel входит в состав [GNU Recutils](https://www.gnu.org/software/recutils/)
и может быть установлена с помощью Guix:

``` shell
guix install recutils
```
