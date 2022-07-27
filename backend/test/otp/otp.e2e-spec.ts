const runOTPTests = (pactum: any) => {
    describe('OTP', () => {
      it(`/otp/request (POST) bad dto`, () => {
        const dto = {
          "intraName" : "wtf"
        }

        return pactum
        .spec()
        .post(`/otp/request`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });

      it(`/otp/request (POST) Bad dto 2`, () => {
        const dto = {
          "email" : "jh_ng@ymail"
        }

        return pactum
        .spec()
        .post(`/otp/request`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });

      it(`/otp/request (POST)`, () => {
        const dto = {
          "email" : "jh_ng@ymail.com"
        }

        jest.setTimeout(600000);
        return pactum
        .spec()
        .post(`/otp/request`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(201)
        .expectBodyContains("key")
      });

      it(`/otp/verify (POST) Bad dto`, () => {
        const dto = {
          "email" : "jh_ng@ymail.com"
        }

        return pactum
        .spec()
        .post(`/otp/verify`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });

      it(`/otp/verify (POST) Bad dto 2`, () => {
        const dto = {
          "check" : "asdfg"
        }

        return pactum
        .spec()
        .post(`/otp/verify`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });

      it(`/otp/verify (POST) Bad dto 3`, () => {
        const dto = {
          "check" : "jh_ng@ymail.com",
          "otp" : "asdf"
        }

        return pactum
        .spec()
        .post(`/otp/verify`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });

      it(`/otp/verify (POST) Bad dto 4`, () => {
        const dto = {
          "check" : "jh_ng@ymail.com",
          "otp" : "asdf",
          "key" : "sadasdasd"
        }

        return pactum
        .spec()
        .post(`/otp/verify`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });

      it(`/otp/verify (POST) Bad dto 5`, () => {
        const dto = {
        }

        return pactum
        .spec()
        .post(`/otp/verify`)
        .withBody(dto)
        .withHeaders({
          Authorization : 'Bearer $S{token}',
        })
        .expectStatus(400)
        .expectBodyContains("error")
      });
  })
}

export default runOTPTests;