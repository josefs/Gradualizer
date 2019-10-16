-module(file).

%% This module contains specs to replace incorrect or inexact specs in OTP.

-type deep_list() :: [char() | atom() | deep_list()].
-type name_all()  :: string() | atom() | deep_list() | (RawFilename :: binary()).
-type posix() ::
        'eacces' | 'eagain' |
        'ebadf' | 'ebadmsg' | 'ebusy' |
        'edeadlk' | 'edeadlock' | 'edquot' |
        'eexist' |
        'efault' | 'efbig' | 'eftype' |
        'eintr' | 'einval' | 'eio' | 'eisdir' |
        'eloop' |
        'emfile' | 'emlink' | 'emultihop' |
        'enametoolong' | 'enfile' |
        'enobufs' | 'enodev' | 'enolck' | 'enolink' | 'enoent' |
        'enomem' | 'enospc' | 'enosr' | 'enostr' | 'enosys' |
        'enotblk' | 'enotdir' | 'enotsup' | 'enxio' |
        'eopnotsupp' | 'eoverflow' |
        'eperm' | 'epipe' |
        'erange' | 'erofs' |
        'espipe'  | 'esrch'  | 'estale' |
        'etxtbsy' |
        'exdev'.

-spec consult(Filename) -> {ok, Terms} | {error, Reason} when
      Filename :: name_all(),
      Terms :: [any()],
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
