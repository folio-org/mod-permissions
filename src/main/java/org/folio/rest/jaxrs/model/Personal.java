
package org.folio.rest.jaxrs.model;

import java.util.HashMap;
import java.util.Map;
import javax.annotation.Generated;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Generated("org.jsonschema2pojo")
@JsonPropertyOrder({
    "full_name",
    "birth_date",
    "phone_home",
    "phone_work",
    "phone_mobile",
    "email_primary",
    "email_alternate",
    "mailing_address"
})
public class Personal {

    @JsonProperty("full_name")
    private String fullName;
    @JsonProperty("birth_date")
    private String birthDate;
    @JsonProperty("phone_home")
    private String phoneHome;
    @JsonProperty("phone_work")
    private String phoneWork;
    @JsonProperty("phone_mobile")
    private String phoneMobile;
    @JsonProperty("email_primary")
    private String emailPrimary;
    @JsonProperty("email_alternate")
    private String emailAlternate;
    @JsonProperty("mailing_address")
    private String mailingAddress;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    /**
     * 
     * @return
     *     The fullName
     */
    @JsonProperty("full_name")
    public String getFullName() {
        return fullName;
    }

    /**
     * 
     * @param fullName
     *     The full_name
     */
    @JsonProperty("full_name")
    public void setFullName(String fullName) {
        this.fullName = fullName;
    }

    public Personal withFullName(String fullName) {
        this.fullName = fullName;
        return this;
    }

    /**
     * 
     * @return
     *     The birthDate
     */
    @JsonProperty("birth_date")
    public String getBirthDate() {
        return birthDate;
    }

    /**
     * 
     * @param birthDate
     *     The birth_date
     */
    @JsonProperty("birth_date")
    public void setBirthDate(String birthDate) {
        this.birthDate = birthDate;
    }

    public Personal withBirthDate(String birthDate) {
        this.birthDate = birthDate;
        return this;
    }

    /**
     * 
     * @return
     *     The phoneHome
     */
    @JsonProperty("phone_home")
    public String getPhoneHome() {
        return phoneHome;
    }

    /**
     * 
     * @param phoneHome
     *     The phone_home
     */
    @JsonProperty("phone_home")
    public void setPhoneHome(String phoneHome) {
        this.phoneHome = phoneHome;
    }

    public Personal withPhoneHome(String phoneHome) {
        this.phoneHome = phoneHome;
        return this;
    }

    /**
     * 
     * @return
     *     The phoneWork
     */
    @JsonProperty("phone_work")
    public String getPhoneWork() {
        return phoneWork;
    }

    /**
     * 
     * @param phoneWork
     *     The phone_work
     */
    @JsonProperty("phone_work")
    public void setPhoneWork(String phoneWork) {
        this.phoneWork = phoneWork;
    }

    public Personal withPhoneWork(String phoneWork) {
        this.phoneWork = phoneWork;
        return this;
    }

    /**
     * 
     * @return
     *     The phoneMobile
     */
    @JsonProperty("phone_mobile")
    public String getPhoneMobile() {
        return phoneMobile;
    }

    /**
     * 
     * @param phoneMobile
     *     The phone_mobile
     */
    @JsonProperty("phone_mobile")
    public void setPhoneMobile(String phoneMobile) {
        this.phoneMobile = phoneMobile;
    }

    public Personal withPhoneMobile(String phoneMobile) {
        this.phoneMobile = phoneMobile;
        return this;
    }

    /**
     * 
     * @return
     *     The emailPrimary
     */
    @JsonProperty("email_primary")
    public String getEmailPrimary() {
        return emailPrimary;
    }

    /**
     * 
     * @param emailPrimary
     *     The email_primary
     */
    @JsonProperty("email_primary")
    public void setEmailPrimary(String emailPrimary) {
        this.emailPrimary = emailPrimary;
    }

    public Personal withEmailPrimary(String emailPrimary) {
        this.emailPrimary = emailPrimary;
        return this;
    }

    /**
     * 
     * @return
     *     The emailAlternate
     */
    @JsonProperty("email_alternate")
    public String getEmailAlternate() {
        return emailAlternate;
    }

    /**
     * 
     * @param emailAlternate
     *     The email_alternate
     */
    @JsonProperty("email_alternate")
    public void setEmailAlternate(String emailAlternate) {
        this.emailAlternate = emailAlternate;
    }

    public Personal withEmailAlternate(String emailAlternate) {
        this.emailAlternate = emailAlternate;
        return this;
    }

    /**
     * 
     * @return
     *     The mailingAddress
     */
    @JsonProperty("mailing_address")
    public String getMailingAddress() {
        return mailingAddress;
    }

    /**
     * 
     * @param mailingAddress
     *     The mailing_address
     */
    @JsonProperty("mailing_address")
    public void setMailingAddress(String mailingAddress) {
        this.mailingAddress = mailingAddress;
    }

    public Personal withMailingAddress(String mailingAddress) {
        this.mailingAddress = mailingAddress;
        return this;
    }

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

    public Personal withAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
        return this;
    }

}
