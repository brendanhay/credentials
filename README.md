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

When deploying modern applications either on-premise or within a cloud
environment such as AWS, you typically need access to a multitude of secrets
such as database credentials, API keys for external third parties, or
credentials for inter-service communication with our micro-service overlords.

One typical erroneous solution to this problem is to store plaintext secrets in source control!

`credentials` is a very simple credential management and
distribution system that uses Amazon Key Management Service (KMS) for key
wrapping and master-key storage, and DynamoDB for credential storage and
sharing.

Please see the [introductory blog post](http://brendanhay.nz/credentials) for more information.


## Setup

1. `stack install credentials-cli`
2. Create a new key in KMS called "credentials". You can do this under Identity
   and Access Management > Encryption Keys in the AWS develoepr console.
3. Make your AWS access credentials available where [amazonka](https://github.com/brendanhay/amazonka) can read them.
   Typically in the `~/.aws/credentials` file, or as `AWS_ACCESS_KEY_ID` and
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
$ credentials put --name foo --secret "A magical secret."
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
$ credentials get --name foo
Retrieving foo from dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
  secret: A magical secret.
```

Additional means of formatting the output and logging suitable for use in shell scripts is
available, see the `--help` text for more information.

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
requires.

#### Read and Write

#### Read Only

## Security Notes


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/credentials/issues).


## Licence

`credentials` is released under the [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
