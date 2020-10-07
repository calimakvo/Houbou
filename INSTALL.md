# ブログ作成ツール Houbou

## Gentooインストール編

### データベース接続設定

#### PostgreSQLインストール

```
# emerge dev-db/postgresql
```

#### 初期化

実際のデータベースバージョンで初期化してください

```
# emerge --config =dev-db/postgresql-12.4
# systemctl start postgresql-12
```

#### データベース接続ユーザー作成

```
$ createuser -U postgres -i -l -S -d devuser -P
新しいロールのためのパスワード: devuser
もう一度入力してください：devuser
```

#### データベース作成

```
$ createdb -U devuser -E UTF-8 -O devuser houbou
```

#### スキーマ初期化

```
$ psql -U devuser houbou -f data/01_schema.sql
```

#### 初期マスタデータ登録

```
$ psql -U devuser houbou -f data/02_initial_insert.sql
```

## nginx

必須ではありません、nginx経由で動作させる場合は設定してください
メディアアップロードサイズを拡張死体場合、Yesodより多少多めに設定する、nginxのサンプル設定はhostingディレクトリを参照してください

```
client_max_body_size 600m;
```

## ビルド

### nodejsインストール

```
# emerge nodejs
```

### javascript関連準備

make buildのため初回だけ実行が必要です

```
$ cd Houbou/jssrc/mediaupload
$ npm install
```

```
$ cd Houbou/jssrc/urlcpy
$ npm install
```

### ビルド

バイナリのビルド

```
$ make build
```

### 実行

開発モードで実行します、localhost:3000 でアクセスできます(yesod devel)

```
$ make run
```

### ログイン

インストール直後は以下の値で設定されています

* http://localhost:3000/admin/auth/login

  * ID: webmaster@example.com
  * Pass: webmaster

## CentOS7インストール編

### Database

#### PostgreSQLインストール

```
# yum install postgresql-server postgresql
```

#### 初期化

```
# su - postgres
$ /usr/bin/initdb -E UTF8 -D /var/lib/pgsql/data/
```

#### サービス起動

外部からアクセスする場合は適切接続設定を確認してください

```
# systemctl start postgresql
```

#### 接続ユーザー作成

Houbouへ接続するユーザーを作成

```
$ createuser -U postgres -i -l -S -d devuser -P
新しいロールのためのパスワード: devuser
もう一度入力してください: devuser
```

#### houbouデータベース作成

```
$ createdb -U devuser -E UTF-8 -O devuser houbou
```

#### Houbouインストール

```
# rpm -Uvh Houbou-0.9.0-1.el7.x86_64.rpm
```

#### 初期スキーマ設定

Houbouリポジトリのdataディレクトリに置いてあります

```
$ psql -U devuser houbou -f data/01_schema.sql
```

#### マスタデータ設定

```
psql -U devuser houbou -f data/02_initial_insert.sql
```

#### アップロードメディア設定

アップロードさせるファイルが保存されるディレクトリを作成し適切なパーミッションを設定

```
# mkdir -p /pub/images
# chown -R houbou:houbou /pub/
```

#### 接続ユーザー設定

作成した接続ユーザーを設定してください

* /etc/sysconfig/houbou

```
YESOD_PGUSER=devuser
YESOD_PGPASS=devuser
YESOD_PGHOST=localhost
YESOD_PGPORT=5432
YESOD_PGDATABASE=houbou
YESOD_PGPOOLSIZE=10
```

#### Houbouサービス起動

```
# systemctl start houbou
```

#### ログイン

初回ログイン後、セッティングを確認してURLやメディア保存先の設定を調整してください

* http://localhost:3000/admin/auth/login

  * ID: webmaster@example.com
  * Pass: webmaster
