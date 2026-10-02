# chat-digest

Read a busy group chat as a digest instead of message by message.

`M-x chat-digest` takes the unread part of a chat, has a language model group
it into threads, and shows the result as an Org buffer: each thread a heading
whose title is its main takeaway, with the points made, who made them, the links
worth opening and what they are, and a link to every message a point rests on.
Approving the digest marks those messages read in the chat itself. Until then
nothing has changed, so a digest can be discarded and made again.

Telegram is the one source so far, through [telega](https://github.com/zevlg/telega.el).

## How a digest is made

- **What it covers.** What follows the chat's read position, up to
  `chat-digest-max-messages` (350), ending where the chat went quiet for
  `chat-digest-lull` (20 minutes) so that no conversation is cut in half.
- **Context.** The model also sees the conversation running into the new
  messages since the last lull, every reply chain they belong to followed up
  to its first message, and the few messages that led into that first message.
  Each reply is shown with the start of the message it answers, as Telegram
  shows it. And it sees the threads earlier digests found, kept per chat as a
  registry of titles and running summaries, so a reply to yesterday's thread
  lands under yesterday's thread.
- **Links.** X posts are read in full through the public
  [fxtwitter](https://github.com/FxEmbed/FxEmbed) API, X Articles included;
  only the post ids leave the machine. Other links without a preview are
  previewed by the source; for Telegram that is Telegram's own servers.
- **The model** is Claude, through the Claude Code CLI, `claude -p`, run in
  the background with every tool, MCP server and settings file switched off and
  with a fixed output schema. It can only answer, so a message in the chat
  that tries to instruct it cannot make it do anything.
- **Announcements**, meaning posts forwarded from a channel and pins, are
  listed apart from the threads, whatever the model makes of them.

## Reading a digest

Each point links its claims to the messages they come from: the words stating
a claim are the link. Messages a point rests on that the model could not tie to
particular words follow it, named by sender and time.

| key     | does                                                     |
|---------|----------------------------------------------------------|
| `TAB`   | open or close the thread at point                        |
| `n` `p` | next, previous heading                                   |
| `RET`   | follow the link at point                                 |
| `a`     | approve: mark the messages read and save the digest      |
| `c`     | approve, then start on the next digest                   |
| `g`     | discard and make the digest again                        |
| `i`     | edit what matters in this chat                           |
| `q`     | leave                                                    |

The same actions are links under the last heading, What next.

A message link opens the message without marking anything read. For Telegram
that is telega's preview mode, which closes the chat while you look around;
opening the chat from telega's root buffer as usual leaves it.

`M-x chat-digest` again while a digest is being made says how far along it is.
`M-x chat-digest-cancel` drops it. `M-x chat-digest-index` lists every chat and
its digests.

## Storage

Everything lives under `chat-digest-directory`, by default
`~/.local/share/chat-digest/`:

```
index.org                         every chat and its digests, by name
telegram/ACCOUNT/CHAT/
    interests.org                 what matters here; the default without it
    state.json                    the thread registry
    digests/2026-09-26-0926.org   one per approved digest
    last-prompt.txt               the most recent request to the model
```

Chats are kept by id, since titles change; `index.org` is where their names are.

## Setup

Needs Emacs 29.1, the `claude` CLI logged in, and, for Telegram, telega.

```elisp
(with-eval-after-load 'telega
  (require 'chat-digest-telegram))
```

## Development

```
make compile    # warnings are errors
make test
```

`dev/replay.el` runs the whole pipeline, the model included, over a recorded
chat; its commentary says how.
