# Credentials

[![Build Status](https://travis-ci.org/brendanhay/credentials.svg?branch=develop)](https://travis-ci.org/brendanhay/credentials)
[![Hackage Version](https://img.shields.io/hackage/v/credentials.svg)](http://hackage.haskell.org/package/credentials)

* [Description](#description)
* [Setup](#setup)
* [Usage](#usage)
    - [Library Usage](#library-usage)
    - [CLI Usage](#cli-usage)
    - [Credential Contexts](#credential-contexts)
    - [Credential Revisions](#credential-revisions)
    - [IAM Policies](#iam-policies)
* [Pricing](#pricing)
* [Security Notes](#security-notes)
* [Contribute](#contribute)
* [Licence](#licence)


## Description

**Warning:** This is an experimental prototype/preview release which is still
in development and not intended for public consumption, caveat emptor!

The `credentials` library and CLI provides a unified interface for managing secure, shared credentials.

It uses [Key Management Service (KMS)](http://aws.amazon.com/kms/) for master key management, locally
encrypts and decrypts secrets, which are then stored in any of the supported
storage backends. (Currently [DynamoDB](http://aws.amazon.com/dynamodb/).)

The use-case is to avoid storing sensitive information such as passwords and
connection strings in plaintext in places such as source control or on
developer machines. Instead you can securely administer and distribute
secrets, leveraging Amazon's IAM policies for access control and permissions to
ensure limited read-only permissions from production/deployed hosts where applicable.

Please see the [introductory blog post](http://brendanhay.nz/credentials) for more information.

If Haskell is not your thing, check out
[credstash](https://github.com/fugue/credstash), the project that inspired
`credentials`.


## Setup

1. `stack install credentials-cli`
2. Create a new key in KMS called "credentials". You can do this under Identity
   and Access Management > Encryption Keys in the AWS developer console.
3. Make your AWS access credentials available where
   [amazonka](https://github.com/brendanhay/amazonka) can find them. Typically
   in the [~/.aws/credentials](https://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs)
   file, or as `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environment variables.
4. `credentials setup`


## Usage

The following is an example of using the `credentials` library as a dependency
of your Haskell project. It retrieves a database connection string containing a
sensitive password, when a webserver starts. It's worth pointing out the setup
all pertains to the underlying
[amazonka](https://github.com/brendanhay/credentials) library, since all of
the `credentials` operations run in a `MonadAWS` context.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens

import Credentials

import Data.ByteString (ByteString)

import Network.AWS
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)

import System.IO (stdout)

main :: IO
main = do
    -- A new 'Logger' to replace the default noop logger is created,
    -- which will print AWS debug information and errors to stdout.
    lgr <- newLogger Debug stdout

    -- A new amazonka 'Env' is created, which auto-discovers the
    -- underlying host credentials.
    env <- newEnv Frankfurt Discover

    let table = Credentials.defaultTable
        key   = Credentials.defaultKeyId
        name  = "secret-database-uri"

    -- We now run the 'AWS' computation with the overriden logger,
    -- performing sequence credentials operation(s).
    -- For 'select', the plaintext and corresponding revision is returned.
    (uri, _) <- runResourceT . runAWS (env & envLogger .~ lgr) $ do
        -- Selecting the credential by name, and specifying 'Nothing' for the
        -- revision results in the latest revision of the credential.
        Credentials.select mempty name Nothing table

    -- We can now connect to the database using our sensitive connection URI.
    run 3000 (app uri)

app :: ByteString -> Application
app uri rq f = ...
```

A basic example of using the CLI is as follows:

```
$ credentials setup
Setting up dynamo:///credentials in eu-central-1.
Running ...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  status: created
```

```
$ credentials insert --name foo --secret "A magical secret."
Writing new revision of foo to dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
```

```
$ credentials select --name foo
Retrieving foo from dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
  secret: A magical secret.
```

```
$ credentials list
Listing contents of dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  foo:
    - 82687c4 # latest
```

Additional means of formatting the output is available, for example the `--format` option
supports `json` (JSON formatting) or `echo` (single shell value) output.

### Credential Contexts

KMS [Encryption Context](http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html) is optionally supported,
which is used to check ciphertext integrity but not stored as part of the encryption parameters.

This means you need to pass the exact context that was used for encryption,
when decrypting, and allows the use of KMS Key Policy and KMS Grant
conditions. One such use would be to ensure your web and database servers can
read a database password, but your database servers cannot read your web
server's private key.

For example, supplying encryption context would look as follows:

```haskell
$ credentials insert --name hello --secret world \
    --context application=db \
    --context environment=production

$ credentials select --name hello \
    --context application=db \
    --context environment=production
```

Failure to specify the exact encryption context resullts in an informative error
during decryption.

### Credential Revisions

Credential storage is immutable and the insertion of a credential into storage is append-only.
When a value for an existing credential name is inserted a unique revision is generated which
you can use in combination with the name to uniquely address any credential in the system.

Rotation of credentials can be performed by simply inserting a new value for
a given credential name, and any system which selects that credential without
a specific revision will always receive the latest revision's value.

You can also specify the exact revision when selecting a credential to perform pinning.

### IAM Policies

The complete list of AWS KMS and DynamoDB permissions the various operations will
require are:

* Setup and teardown:
    - `CreateTable`
    - `DestroyTable`
    - `ListTables`
* List credentials and revisions:
    - `Scan`
* Insert a new encrypted credential revision:
    - `GenerateDataKey`
    - `Query`
    - `PutItem`
* Select and decrypt an existing credential revision:
    - `Decrypt`
    - `Query`
* Deletion of a credential revision, or revisions:
    - `Query`
    - `DeleteItem`

It's recommended you allow only the minimal set of permissions as your usecase
requires. For example, the following two IAM policies illustrate the minimum
allowable API operations for read/write, and read only access respectively:

**Read and Write**

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "kms:GenerateDataKey"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:kms:us-east-1:AWS_ACCOUNT_ID:alias/credentials"
    },
    {
      "Action": [
        "dynamodb:PutItem",
        "dynamodb:Query"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:us-east-1:AWS_ACCOUNT_ID:table/credentials"
    }
  ]
}
```

**Read Only**

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "kms:Decrypt"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:kms:us-east-1:AWS_ACCOUNT_ID:alias/credentials"
    },
    {
      "Action": [
        "dynamodb:Query"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:us-east-1:AWS_ACCOUNT_ID:table/credentials"
    }
  ]
}
```

Remember to replace the use of `AWS_ACCOUNT_ID` above with your own account identifier.

## Pricing

A single master key in KMS costs $1 USD per month. The DynamoDB table throughput
is configured to use 1 provisioned read and 1 provisioned write, so if you are using
less than the free tier limit of 25 reads and 25 writes per second, only the KMS
charges will apply.


## Security Notes


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/credentials/issues).


## Licence

`credentials` is released under the [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
