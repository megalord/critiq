'use strict';

exports.plugin = function () {
  if (plugin) {
    return plugin;
  } else {
    throw new Error('Module not loaded inside neovim node-host environment.');
  }
};

exports.commandSetLine = function (name) {
  return function (content) {
    return function () {
      plugin.commandSync(name, function (nvim, cb) {
        try {
          nvim.setCurrentLine(content, cb);
        } catch (err) {
          cb(err);
        }
      });
      return {}
    }
  }
}
