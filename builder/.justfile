# Normally, if a command returns a non-zero exit status, execution will stop.

# Typechecks typescript files
check:
    npx tsc --noEmit
    npx prettier --write src
