exports.collapseStream = function (stream) {
  return function (onError) {
    return function (onSuccess) {
      return function () {
        var body = '';
        stream.on('data', function (chunk) {
          body += chunk;
        });
        stream.on('end', function () {
          onSuccess(body)();
        });
        stream.on('error', function (err) {
          onError(err)();
        });
        return {};
      }
    }
  }
};
