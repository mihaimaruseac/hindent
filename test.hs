openTextDocumentRequest
  :: TextDocumentIdentifier
  -> SentRequestWrapper "vsch/registerTextDocumentContentProvider" True TextDocumentIdentifier Void Void
openTextDocumentRequest = SentRequestWrapper
