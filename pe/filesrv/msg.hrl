%% This is the standard set of file server messages.

% upload sends data to be uploaded.
-record(upload, {client, data}).

% tag marks an ID as having an alternate name.
-record(tag, {client, id, name}).

% put combines upload and tag.
-record(put, {client, name, data}).

% retrieve obtains the data referenced by id.
-record(retrieve, {client, id}).

% fetch obtains the data referenced by an alternative name.
-record(fetch, {client, name}).

% stop tells the server to shutdown.
-record(stop, {client}).

% dump tells the server to send the current state of the system.
-record(dump, {client}).
