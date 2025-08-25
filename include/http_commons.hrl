%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2025 Xentelar Advanced Technologies. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @doc Response headers
%% @end
%% -----------------------------------------------------------------------------
-define(DEFAULT_HEADER,   #{<<"content-type">> => <<"application/json;charset=UTF-8">>}).
-define(IMAGE_HEADER,     #{<<"content-type">> => <<"image/png">>}).

%% -----------------------------------------------------------------------------
%% @doc Error messages
%% @end
%% -----------------------------------------------------------------------------
-define(TOKEN_NOT_FOUNT,          "token.not.fount").

%% -----------------------------------------------------------------------------
%% @doc HTTP methods
%% @end
%% -----------------------------------------------------------------------------
-define(GET,      <<"GET">>).
-define(POST,     <<"POST">>).
-define(PUT,      <<"PUT">>).
-define(PATCH,    <<"PATCH">>).
-define(DELETE,   <<"DELETE">>).
-define(OPTIONS,  <<"OPTIONS">>).

%% -----------------------------------------------------------------------------
%% @doc Access control headers
%% @end
%% -----------------------------------------------------------------------------
-define(ACCESS_CONTROL_HEADERS(Origin, Methods), #{
		<<"Access-Control-Allow-Origin">> => Origin,
    <<"Access-Control-Allow-Methods">> => Methods,
    <<"Access-Control-Allow-Headers">> => <<"*">>,
    <<"Access-Control-Expose-Headers">> => <<"*">>,
    <<"Access-Control-Allow-Credentials">> => <<"include">>,
    <<"Access-Control-Max-Age">> => <<"10">>
	}).

%% -----------------------------------------------------------------------------
%% @doc HTTP codes
%% @end
%% -----------------------------------------------------------------------------
-define(OK_HTTP_CODE,                               200).
-define(CREATED_HTTP_CODE,                          201).
-define(ACCEPTED_HTTP_CODE,                         202).
-define(NON_AUTHORITATIVE_INFORMATION_HTTP_CODE,    203).
-define(NO_CONTENT_HTTP_CODE,                       204).
-define(RESET_CONTENT_HTTP_CODE,                    205).
-define(PARTIAL_CONTENT_HTTP_CODE,                  206).
-define(MULTI_STATUS_HTTP_CODE,                     207).
-define(ALREADY_REPORTED_HTTP_CODE,                 208).
-define(IM_USED_HTTP_CODE,                          226).

-define(MULTIPLE_CHOICES_HTTP_CODE,                 300).
-define(MOVED_PERMANENTLY_HTTP_CODE,                301).
-define(FOUND_HTTP_CODE,                            302).
-define(SEE_OTHER_HTTP_CODE,                        303).
-define(NOT_MODIFIED_HTTP_CODE,                     304).
-define(USE_PROXY_HTTP_CODE,                        305).
-define(TEMPORARY_REDIRECT_HTTP_CODE,               307).
-define(PERMANENT_REDIRECT_HTTP_CODE,               308).

-define(BAD_REQUEST_HTTP_CODE,                      400).
-define(UNAUTHORIZED_HTTP_CODE,                     401).
-define(PAYMENT_REQUIRED_HTTP_CODE,                 402).
-define(FORBIDDEN_HTTP_CODE,                        403).
-define(NOT_FOUND_HTTP_CODE,                        404).
-define(METHOD_NOT_ALLOWED_HTTP_CODE,               405).
-define(NOT_ACCEPTABLE_HTTP_CODE,                   406).
-define(PROXY_AUTHENTICATION_REQUIRED_HTTP_CODE,    407).
-define(REQUEST_TIMEOUT_HTTP_CODE,                  408).
-define(CONFLICT_HTTP_CODE,                         409).
-define(GONE_HTTP_CODE,                             410).
-define(LENGTH_REQUIRED_HTTP_CODE,                  411).
-define(PRECONDITION_FAILED_HTTP_CODE,              412).
-define(PAYLOAD_TOO_LARGE_HTTP_CODE,                413).
-define(URI_TOO_LONG_HTTP_CODE,                     414).
-define(UNSUPPORTED_MEDIA_TYPE_HTTP_CODE,           415).
-define(RANGE_NOT_SATISFIABLE_HTTP_CODE,            416).
-define(EXPECTATION_FAILED_HTTP_CODE,               417).
-define(MISDIRECTED_REQUEST_HTTP_CODE,              421).
-define(UNPROCESSABLE_ENTITY_HTTP_CODE,             422).
-define(LOCKED_HTTP_CODE,                           423).
-define(FAILED_DEPENDENCY_HTTP_CODE,                424).
-define(TOO_EARLY_HTTP_CODE,                        425).
-define(UPGRADE_REQUIRED_HTTP_CODE,                 426).
-define(PRECONDITION_REQUIRED_HTTP_CODE,            428).
-define(TOO_MANY_REQUESTS_HTTP_CODE,                429).
-define(REQUEST_HEADER_FIELDS_TOO_LARGE_HTTP_CODE,  431).
-define(UNAVAILABLE_FOR_LEGAL_REASONS_HTTP_CODE,    451).

-define(INTERNAL_SERVER_ERROR_HTTP_CODE,            500).
-define(NOT_IMPLEMENTED_HTTP_CODE,                  501).
-define(BAD_GATEWAY_HTTP_CODE,                      502).
-define(SERVICE_UNAVAILABLE_HTTP_CODE,              503).
-define(GATEWAY_TIMEOUT_HTTP_CODE,                  504).
-define(HTTP_VERSION_NOT_SUPPORTED_HTTP_CODE,       505).
-define(VARIANT_ALSO_NEGOTIATES_HTTP_CODE,          506).
-define(INSUFFICIENT_STORAGE_HTTP_CODE,             507).
-define(LOOP_DETECTED_HTTP_CODE,                    508).
-define(NOT_EXTENDED_HTTP_CODE,                     510).
-define(NETWORK_AUTHENTICATION_REQUIRED_HTTP_CODE,  511).

-define(HTTP_MSG_OK,                               {?OK_HTTP_CODE, <<"Ok">>}).
-define(HTTP_MSG_CREATED,                          {?CREATED_HTTP_CODE, <<"The request has been fulfilled">>}).
-define(HTTP_MSG_ACCEPTED,                         {?ACCEPTED_HTTP_CODE, <<"The request has been accepted for processing">>}).
-define(HTTP_MSG_NON_AUTHORITATIVE_INFORMATION,    {?NON_AUTHORITATIVE_INFORMATION_HTTP_CODE, <<"The server is a transforming proxy that received a 200 OK from its origin">>}).
-define(HTTP_MSG_NO_CONTENT,                       {?NO_CONTENT_HTTP_CODE, <<"The server successfully processed the request">>}).
-define(HTTP_MSG_RESET_CONTENT,                    {?RESET_CONTENT_HTTP_CODE, <<"The server successfully processed the request">>}).
-define(HTTP_MSG_PARTIAL_CONTENT,                  {?PARTIAL_CONTENT_HTTP_CODE, <<"The server is delivering only part of the resource due to a range header sent by the client">>}).
-define(HTTP_MSG_MULTI_STATUS,                     {?MULTI_STATUS_HTTP_CODE, <<"Multi-Status (WebDAV; RFC 4918)">>}).
-define(HTTP_MSG_ALREADY_REPORTED,                 {?ALREADY_REPORTED_HTTP_CODE, <<"Already Reported (WebDAV; RFC 5842)">>}).
-define(HTTP_MSG_IM_USED,                          {?IM_USED_HTTP_CODE, <<"IM Used (RFC 3229)">>}).

-define(HTTP_MSG_MULTIPLE_CHOICES,                 {?MULTIPLE_CHOICES_HTTP_CODE, <<"Multiple Choices">>}).
-define(HTTP_MSG_MOVED_PERMANENTLY,                {?MOVED_PERMANENTLY_HTTP_CODE, <<"Moved Permanently">>}).
-define(HTTP_MSG_FOUND,                            {?FOUND_HTTP_CODE, <<"Found (Previously 'Moved temporarily')">>}).
-define(HTTP_MSG_SEE_OTHER,                        {?SEE_OTHER_HTTP_CODE, <<"The response to the request can be found under another URI ">>}).
-define(HTTP_MSG_NOT_MODIFIED,                     {?NOT_MODIFIED_HTTP_CODE, <<"The resource has not been modified since the version specified by the request headers">>}).
-define(HTTP_MSG_USE_PROXY,                        {?USE_PROXY_HTTP_CODE, <<"The requested resource is available only through a proxy">>}).
-define(HTTP_MSG_TEMPORARY_REDIRECT,               {?TEMPORARY_REDIRECT_HTTP_CODE, <<"Temporary Redirect">>}).
-define(HTTP_MSG_PERMANENT_REDIRECT,               {?PERMANENT_REDIRECT_HTTP_CODE, <<"Permanent Redirect">>}).

-define(HTTP_MSG_BAD_REQUEST,                      {?BAD_REQUEST_HTTP_CODE, <<"Bad Request">>}).
-define(HTTP_MSG_UNAUTHORIZED,                     {?UNAUTHORIZED_HTTP_CODE, <<"Unauthorized">>}).
-define(HTTP_MSG_PAYMENT_REQUIRED,                 {?PAYMENT_REQUIRED_HTTP_CODE, <<"Payment Required">>}).
-define(HTTP_MSG_FORBIDDEN,                        {?FORBIDDEN_HTTP_CODE, <<"The user not having the necessary permissions for a resource">>}).
-define(HTTP_MSG_NOT_FOUND,                        {?NOT_FOUND_HTTP_CODE, <<"Resource not Found">>}).
-define(HTTP_MSG_METHOD_NOT_ALLOWED,               {?METHOD_NOT_ALLOWED_HTTP_CODE, <<"Method Not Allowed">>}).
-define(HTTP_MSG_NOT_ACCEPTABLE,                   {?NOT_ACCEPTABLE_HTTP_CODE, <<"Not Acceptable">>}).
-define(HTTP_MSG_PROXY_AUTHENTICATION_REQUIRED,    {?PROXY_AUTHENTICATION_REQUIRED_HTTP_CODE, <<"Proxy Authentication Required">>}).
-define(HTTP_MSG_REQUEST_TIMEOUT,                  {?REQUEST_TIMEOUT_HTTP_CODE, <<"Request Timeout">>}).
-define(HTTP_MSG_CONFLICT,                         {?CONFLICT_HTTP_CODE, <<"Conflict">>}).
-define(HTTP_MSG_GONE,                             {?GONE_HTTP_CODE, <<"Gone">>}).
-define(HTTP_MSG_LENGTH_REQUIRED,                  {?LENGTH_REQUIRED_HTTP_CODE, <<"Length Required">>}).
-define(HTTP_MSG_PRECONDITION_FAILED,              {?PRECONDITION_FAILED_HTTP_CODE, <<"Precondition Failed">>}).
-define(HTTP_MSG_PAYLOAD_TOO_LARGE,                {?PAYLOAD_TOO_LARGE_HTTP_CODE, <<"Payload Too Large">>}).
-define(HTTP_MSG_URI_TOO_LONG,                     {?URI_TOO_LONG_HTTP_CODE, <<"URI Too Long">>}).
-define(HTTP_MSG_UNSUPPORTED_MEDIA_TYPE,           {?UNSUPPORTED_MEDIA_TYPE_HTTP_CODE, <<"Unsupported Media Type">>}).
-define(HTTP_MSG_RANGE_NOT_SATISFIABLE,            {?RANGE_NOT_SATISFIABLE_HTTP_CODE, <<"Range Not Satisfiable">>}).
-define(HTTP_MSG_EXPECTATION_FAILED,               {?EXPECTATION_FAILED_HTTP_CODE, <<"Expectation Failed">>}).
-define(HTTP_MSG_MISDIRECTED_REQUEST,              {?MISDIRECTED_REQUEST_HTTP_CODE, <<"Misdirected Request">>}).
-define(HTTP_MSG_UNPROCESSABLE_ENTITY,             {?UNPROCESSABLE_ENTITY_HTTP_CODE, <<"Unprocessable Entity">>}).
-define(HTTP_MSG_LOCKED,                           {?LOCKED_HTTP_CODE, <<"Locked (WebDAV; RFC 4918)">>}).
-define(HTTP_MSG_FAILED_DEPENDENCY,                {?FAILED_DEPENDENCY_HTTP_CODE, <<"Failed Dependency (WebDAV; RFC 4918)">>}).
-define(HTTP_MSG_TOO_EARLY,                        {?TOO_EARLY_HTTP_CODE, <<"Too Early (RFC 8470)">>}).
-define(HTTP_MSG_UPGRADE_REQUIRED,                 {?UPGRADE_REQUIRED_HTTP_CODE, <<"Upgrade Required">>}).
-define(HTTP_MSG_PRECONDITION_REQUIRED,            {?PRECONDITION_REQUIRED_HTTP_CODE, <<"Precondition Required (RFC 6585)">>}).
-define(HTTP_MSG_TOO_MANY_REQUESTS,                {?TOO_MANY_REQUESTS_HTTP_CODE, <<"Too Many Requests (RFC 6585)">>}).
-define(HTTP_MSG_REQUEST_HEADER_FIELDS_TOO_LARGE,  {?REQUEST_HEADER_FIELDS_TOO_LARGE_HTTP_CODE, <<"Request Header Fields Too Large (RFC 6585)">>}).
-define(HTTP_MSG_UNAVAILABLE_FOR_LEGAL_REASONS,    {?UNAVAILABLE_FOR_LEGAL_REASONS_HTTP_CODE, <<"Unavailable For Legal Reasons (RFC 7725)">>}).

-define(HTTP_MSG_INTERNAL_SERVER_ERROR,            {?INTERNAL_SERVER_ERROR_HTTP_CODE, <<"Internal Server Error">>}).
-define(HTTP_MSG_NOT_IMPLEMENTED,                  {?NOT_IMPLEMENTED_HTTP_CODE, <<"Not Implemented">>}).
-define(HTTP_MSG_BAD_GATEWAY,                      {?BAD_GATEWAY_HTTP_CODE, <<"Bad Gateway">>}).
-define(HTTP_MSG_SERVICE_UNAVAILABLE,              {?SERVICE_UNAVAILABLE_HTTP_CODE, <<"Service Unavailable">>}).
-define(HTTP_MSG_GATEWAY_TIMEOUT,                  {?GATEWAY_TIMEOUT_HTTP_CODE, <<"Gateway Timeout">>}).
-define(HTTP_MSG_HTTP_VERSION_NOT_SUPPORTED,       {?HTTP_VERSION_NOT_SUPPORTED_HTTP_CODE, <<"HTTP Version Not Supported">>}).
-define(HTTP_MSG_VARIANT_ALSO_NEGOTIATES,          {?VARIANT_ALSO_NEGOTIATES_HTTP_CODE, <<"Variant Also Negotiates (RFC 2295)">>}).
-define(HTTP_MSG_INSUFFICIENT_STORAGE,             {?INSUFFICIENT_STORAGE_HTTP_CODE, <<"Insufficient Storage (WebDAV; RFC 4918)">>}).
-define(HTTP_MSG_LOOP_DETECTED,                    {?LOOP_DETECTED_HTTP_CODE, <<"Loop Detected (WebDAV; RFC 5842)">>}).
-define(HTTP_MSG_NOT_EXTENDED,                     {?NOT_EXTENDED_HTTP_CODE, <<"Not Extended (RFC 2774)">>}).
-define(HTTP_MSG_NETWORK_AUTHENTICATION_REQUIRED,  {?NETWORK_AUTHENTICATION_REQUIRED_HTTP_CODE, <<"Network Authentication Required (RFC 6585)">>}).


-define(HTTP_DEFAULT_MSGS, [?HTTP_MSG_OK,
                            ?HTTP_MSG_CREATED,
                            ?HTTP_MSG_ACCEPTED,
                            ?HTTP_MSG_NON_AUTHORITATIVE_INFORMATION,
                            ?HTTP_MSG_NO_CONTENT,
                            ?HTTP_MSG_RESET_CONTENT,
                            ?HTTP_MSG_PARTIAL_CONTENT,
                            ?HTTP_MSG_MULTI_STATUS,
                            ?HTTP_MSG_ALREADY_REPORTED,
                            ?HTTP_MSG_IM_USED,

                            ?HTTP_MSG_MULTIPLE_CHOICES,
                            ?HTTP_MSG_MOVED_PERMANENTLY,
                            ?HTTP_MSG_FOUND,
                            ?HTTP_MSG_SEE_OTHER,
                            ?HTTP_MSG_NOT_MODIFIED,
                            ?HTTP_MSG_USE_PROXY,
                            ?HTTP_MSG_TEMPORARY_REDIRECT,
                            ?HTTP_MSG_PERMANENT_REDIRECT,

                            ?HTTP_MSG_BAD_REQUEST,
                            ?HTTP_MSG_UNAUTHORIZED,
                            ?HTTP_MSG_PAYMENT_REQUIRED,
                            ?HTTP_MSG_FORBIDDEN,
                            ?HTTP_MSG_NOT_FOUND,
                            ?HTTP_MSG_METHOD_NOT_ALLOWED,
                            ?HTTP_MSG_NOT_ACCEPTABLE,
                            ?HTTP_MSG_PROXY_AUTHENTICATION_REQUIRED,
                            ?HTTP_MSG_REQUEST_TIMEOUT,
                            ?HTTP_MSG_CONFLICT,
                            ?HTTP_MSG_GONE,
                            ?HTTP_MSG_LENGTH_REQUIRED,
                            ?HTTP_MSG_PRECONDITION_FAILED,
                            ?HTTP_MSG_PAYLOAD_TOO_LARGE,
                            ?HTTP_MSG_URI_TOO_LONG,
                            ?HTTP_MSG_UNSUPPORTED_MEDIA_TYPE,
                            ?HTTP_MSG_RANGE_NOT_SATISFIABLE,
                            ?HTTP_MSG_EXPECTATION_FAILED,
                            ?HTTP_MSG_MISDIRECTED_REQUEST,
                            ?HTTP_MSG_UNPROCESSABLE_ENTITY,
                            ?HTTP_MSG_LOCKED,
                            ?HTTP_MSG_FAILED_DEPENDENCY,
                            ?HTTP_MSG_TOO_EARLY,
                            ?HTTP_MSG_UPGRADE_REQUIRED,
                            ?HTTP_MSG_PRECONDITION_REQUIRED,
                            ?HTTP_MSG_TOO_MANY_REQUESTS,
                            ?HTTP_MSG_REQUEST_HEADER_FIELDS_TOO_LARGE,
                            ?HTTP_MSG_UNAVAILABLE_FOR_LEGAL_REASONS,

                            ?HTTP_MSG_INTERNAL_SERVER_ERROR,
                            ?HTTP_MSG_NOT_IMPLEMENTED,
                            ?HTTP_MSG_BAD_GATEWAY,
                            ?HTTP_MSG_SERVICE_UNAVAILABLE,
                            ?HTTP_MSG_GATEWAY_TIMEOUT,
                            ?HTTP_MSG_HTTP_VERSION_NOT_SUPPORTED,
                            ?HTTP_MSG_VARIANT_ALSO_NEGOTIATES,
                            ?HTTP_MSG_INSUFFICIENT_STORAGE,
                            ?HTTP_MSG_LOOP_DETECTED,
                            ?HTTP_MSG_NOT_EXTENDED,
                            ?HTTP_MSG_NETWORK_AUTHENTICATION_REQUIRED]).
