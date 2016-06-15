console.log('hello');
plugin.commandSync('Foo', (nvim, cb) => {
  try {
    nvim.setCurrentLine('foo', cb);
  } catch (err) {
    cb(err);
  }
})
