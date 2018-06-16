# apus

apus is a wiki system which features instant updates. Namely, your changes are broadcasted to other clients.

# Get started

Copy `config.yaml.sample` as `config.yaml`.

Create an OAuth app on [GitHub](https://github.com/settings/developers) and obtain a client id and secret, then set `clientId` and `clientSecret` fields in `config.yaml`.

Prepare a SSL certificate and a key, and replace the values of `tlsCertificate` and `tlsKey` with the paths.

Now you can start a server:

```
stack build
stack exec apus-exe config.yaml
```
