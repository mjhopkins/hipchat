# hipchat

Haskell bindings to the [HipChat](https://www.hipchat.com) REST API and helpers for writing a HipChat add-on.

## An overview of the HipChat API and HipChat add-on API (extension points)

* API
  * Capabilities API
  * Emoticons API
  * Extensions API
  * Groups API
  * Integrations API
  * Invites API
  * OAuth sessions API
  * Prefs API
  * Rooms API
  * Users API
* Extension points
  * Capabilities descriptor
  * Actions
  * Configuration pages
  * Dialogs
  * External pages
  * Glances
  * Webhooks
  * Web panels

There is a [JSON-schema](https://api.hipchat.com/v2/capabilities/addon/schema?draft=v4)
which documents the form an add-on capabilities descriptor should take.
