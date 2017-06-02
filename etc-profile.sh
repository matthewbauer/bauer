export PREFIX=@out@

if [ -d @out@/etc/profile.d ]; then
  for i in @out@/etc/profile.d/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
fi
