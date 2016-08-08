# Credentials

[![Build Status](https://travis-ci.org/brendanhay/credentials.svg?branch=develop)](https://travis-ci.org/brendanhay/credentials)
[![Hackage Version](https://img.shields.io/hackage/v/credentials.svg)](http://hackage.haskell.org/package/credentials)

* [Description](#description)
* [Setup](#setup)
* [Usage](#usage)
    - [Credential Storage](#credential-storage)
    - [Credential Retrieval](#credential-retrieval)
    - [Credential Management and Auditing](#credential-management-and-auditing)
    - [Credential Revisions](#credential-revisions)
    - [IAM Policies](#iam-policies)
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


## Setup

1. `stack install credentials-cli`
2. Create a new key in KMS called "credentials". You can do this under Identity
   and Access Management > Encryption Keys in the AWS developer console.
3. Make your AWS access credentials available where
   [amazonka](https://github.com/brendanhay/amazonka) can find them. Typically
   in the `~/.aws/credentials` file, or as `AWS_ACCESS_KEY_ID` and
   `AWS_SECRET_ACCESS_KEY` environment variables.
4. `credentials setup`


## Usage

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
$ credentials list
Listing contents of dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  foo:
    - 82687c4 # latest
```

```
$ credentials select --name foo
Retrieving foo from dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
  secret: A magical secret.
```

Additional means of formatting the output and logging suitable for use in shell scripts is
available, see the `--help` text for more information.

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


### Credential Storage

### Credential Retrieval

### Credential Management and Auditing

[Encryption Context](http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html)

### Credential Revisions

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

## Security Notes


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/credentials/issues).


## Licence

`credentials` is released under the [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
