local ffi = require('ffi')

local M = {}

local ssl = ffi.load('ssl.so')

ffi.cdef([[
typedef struct EVP_MD EVP_MD;

enum { EVP_MAX_MD_SIZE = 64 };

const EVP_MD *EVP_sha256(void);
int EVP_Digest(const void *, size_t, unsigned char *, unsigned int *, const EVP_MD *, void *);
int OPENSSL_buf2hexstr_ex(char *, size_t, size_t *, const unsigned char *, long, const char);
]])

local md, md_size =
	ffi.new('unsigned char[?]', ssl.EVP_MAX_MD_SIZE), ffi.new('int[1]')
local buf, buf_size =
	ffi.new('unsigned char[?]', ssl.EVP_MAX_MD_SIZE * 2 + 1), ffi.new('size_t[1]')

local function generic(data, evp_type)
	assert(ssl.EVP_Digest(data, #data, md, md_size, evp_type, nil) == 1)
	assert(
		ssl.OPENSSL_buf2hexstr_ex(buf, ffi.sizeof(buf), buf_size, md, md_size[0], 0)
			== 1
	)
	return ffi.string(buf, buf_size[0] - 1 --[[ NUL ]])
end

do
	local evp_type = ssl.EVP_sha256()
	function M.sha256(data)
		return generic(data, evp_type)
	end
end

return M
