# Normally, if a command returns a non-zero exit status, execution will stop.

# Typechecks typescript files
check:
    npx tsc --noEmit
    npx prettier --write src

clean:
    rm -rf {{justfile_directory()}}/dist
    rm -rf {{justfile_directory()}}/.parcel-cache
